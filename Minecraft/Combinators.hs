{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
-- http://www.timphilipwilliams.com/posts/2019-07-25-minecraft.html
--
-- FIXME: much of this code is likely based on the assumption that we are using absolute coordinates

module Minecraft.Combinators where

import           Prelude hiding (repeat, replicate, floor)
import           Control.Arrow
import           Control.Lens (Lens, Lens', (^.), (&), (.~), view, over, set, makeLenses, mapped)
import           Control.Monad
import           Data.Int (Int32)
import qualified Data.Text.Lazy.IO as LT
import qualified Data.Text.Lazy.Builder as B
import qualified Data.Map as Map
import           Minecraft.Block
import           Minecraft.Command (Command(SetBlock), OldBlockHandling)
import           Minecraft.Core (Axis(..), Pos(..), PosKind(..), XYZ(..), xyz, x, y ,z, origin, posKind, posValue, toTilda, toCaret, render, ab)
import           System.Directory (createDirectoryIfMissing)
import           System.FilePath
import           System.IO
import           System.Random (randomIO)

-- | A single block, placed at an XYZ position
data BlockXYZ = BlockXYZ
  { _blockXYZ  :: XYZ
  , _blockKind :: Block
  , _blockOldHandling :: Maybe OldBlockHandling
  }
  deriving (Eq, Show)
makeLenses ''BlockXYZ

-- | a collection of positioned Blocks
newtype Blocks = Blocks { _blocks :: [BlockXYZ] }
    deriving (Eq, Semigroup, Monoid, Show)
makeLenses ''Blocks

-- | map `toTilda` over the Blocks
toTilda' :: Blocks -> Blocks
toTilda' = (over (blocks . mapped . blockXYZ) toTilda)

-- | map `toCaret` over the Blocks
toCaret' :: Blocks -> Blocks
toCaret' = (over (blocks . mapped . blockXYZ) toCaret)

-- | make Cobblestone blocks at the supplied XYZ coordinates
mkBlocks :: [XYZ] -> Blocks
mkBlocks = Blocks . map (\c -> BlockXYZ c Cobblestone Nothing)

-- | A block of nothing (air) at the origin (0,0,0)
zero :: Blocks
zero = Blocks [BlockXYZ origin Air Nothing]

-- | map a function @(BlockXYZ -> BlockXYZ)@ over the 'Blocks'
mapBlocks :: (BlockXYZ -> BlockXYZ) -> Blocks -> Blocks
mapBlocks f = Blocks . map f . _blocks

-- | map a function @(Block -> Block)@ over the 'Blocks'
mapKind :: (Block -> Block) -> Blocks -> Blocks
mapKind f = mapBlocks $ over blockKind f

-- | filter blocks
filterBlocks :: (BlockXYZ -> Bool) -- ^ predicate. If it returns 'False' the block is removed
             -> Blocks
             -> Blocks
filterBlocks f = Blocks . filter f . _blocks

-- | assign each block a sequential number starting at 0 and then
-- apply the filter predicate to those block numbers.
--
-- This is useful for doing things like dropping every other block
filterN :: (Int -> Bool) -> Blocks -> Blocks
filterN f (Blocks blks) =
  Blocks $ map snd $ filter (\(n, _) -> f n) $ zip [0..] blks

-- | Set the kind of all blocks
infixr 8 #
(#) :: Blocks -> Block -> Blocks
(#) blocks k = mapKind (const k) blocks

-- | We will abstract over dimensions using lenses:
type Dimension = Lens' XYZ Pos

-- | A plane define my two dimensions
data Plane = Plane Dimension Dimension

-- | standard planes
--
-- While 'xzPlane' might sound more natural, we want the cross product
-- of the two dimensions to be a vector in the same direction as the
-- 3rd.
xyPlane, yzPlane, zxPlane :: Plane
xyPlane = Plane x y
yzPlane = Plane y z
zxPlane = Plane z x

-- | Move blocks by @i@ in dimension @d@.
move :: Dimension -> Int32 -> Blocks -> Blocks
move d i = mapBlocks $ over (blockXYZ . d . posValue) (+i)

-- | Translate blocks by the supplied @x, y, z@ offset.
translate :: Int32 -> Int32 -> Int32 -> Blocks -> Blocks
translate x' y' z' = move x x' . move y y' . move z z'

-- | rotate 90° clockwise around the axis at the origin
-- FIMXE: instead of providing an axis could we provide a plane ?
rotate_90' :: Axis -> Blocks -> Blocks
rotate_90' X = mapBlocks $ over blockXYZ $ \(XYZ x (Pos pk y) z) ->
  (XYZ x z (Pos pk (-y)))
rotate_90' Y = mapBlocks $ over blockXYZ $ \(XYZ x y (Pos px z)) ->
  (XYZ (Pos px (-z)) y x)
rotate_90' Z = mapBlocks $ over blockXYZ $ \(XYZ (Pos pk x) y z) ->
  (XYZ y (Pos pk (-x)) z)

-- | rotate 90° clockwise around the origin around the axis normal to the plane
rotate_90 :: Plane -> Blocks -> Blocks
rotate_90 (Plane d1 d2) blks =
  mapBlocks (over blockXYZ $ \xyz -> xyz & d1 .~ (xyz ^. d2)
                                         & d2 .~ over posValue negate (xyz ^. d1)
            ) blks

{-
rotate_90 d1 d2 blks = mapBlocks (over blockXYZ $ \xyz -> xyz & d1 .~ over posValue negate (xyz ^. d2)
                                                              & d2 .~ (xyz ^. d1)
                                  ) blks
-}

-- | rotate 180° around the origin around the Axis
rotate_180' :: Axis -> Blocks -> Blocks
rotate_180' X = mapBlocks $ over blockXYZ $ \(XYZ x (Pos pky y) (Pos pkz z)) ->
  (XYZ x (Pos pky (-y)) (Pos pkz (-z)))
rotate_180' Y = mapBlocks $ over blockXYZ $ \(XYZ (Pos pkx x) y (Pos pkz z)) ->
  (XYZ (Pos pkx (-x)) y (Pos pkz (-z)))
rotate_180' Z = mapBlocks $ over blockXYZ $ \(XYZ (Pos pkx x) (Pos pky y) z) ->
  (XYZ (Pos pkx (-x)) (Pos pky (-y)) z)

-- | rotate 180° around the origin around the axis normal to the plane
rotate_180 :: Plane -> Blocks -> Blocks
rotate_180 (Plane d1 d2) blks =
  mapBlocks (over blockXYZ $ over (d2 . posValue) negate . over (d1 . posValue) negate) blks


-- | rotate 180° around the axis at the origin
rotate_270' :: Axis -> Blocks -> Blocks
rotate_270' X = mapBlocks $ over blockXYZ $ \(XYZ x y (Pos pk z)) ->
  (XYZ x (Pos pk (-z)) y)
rotate_270' Y = mapBlocks $ over blockXYZ $ \(XYZ (Pos pk x) y z) ->
  (XYZ z y (Pos pk (-x)))
rotate_270' Z = mapBlocks $ over blockXYZ $ \(XYZ x (Pos pk y) z) ->
  (XYZ (Pos pk (-y)) x z)

-- | rotate 270° clockwise around the origin around the axis normal to the plane
rotate_270 :: Plane -> Blocks -> Blocks
rotate_270 (Plane d1 d2) blks =
  mapBlocks (over blockXYZ $ \xyz -> xyz & d1 .~ over posValue negate (xyz ^. d2)
                                         & d2 .~ (xyz ^. d1)
            ) blks

-- | mirror along dimension
--
-- If the the 'Bool' is true then we get the original blocks +
-- mirrored blocks. If it is false, then only the mirror image is
-- returned.
--
mirror :: Dimension
       -> Bool -- ^ include original blocks in output
       -> Blocks
       -> Blocks
mirror d includeOrig orig =
  let new = mapBlocks (over (blockXYZ . d . posValue) (negate)) orig
  in case includeOrig of
    True   -> orig <> new
    False -> new

-- FIXME: does this do something sensible with tilda/caret notation?
-- | swap the 'y' and 'z' coordinates
swap_yz :: Blocks -> Blocks
swap_yz = mapBlocks $ over blockXYZ $ \xyz ->
             case xyz of
               (XYZ x y z)  -> XYZ x z y

-- | swap the 'x' and 'y' coordinates
swap_xy :: Blocks -> Blocks
swap_xy = mapBlocks $ over blockXYZ $ \xyz ->
             case xyz of
               (XYZ x y z) -> XYZ y x z

-- | Get the coordinate bounds for blocks along a particular dimension 'd'.
bounds :: Dimension -> Blocks -> (Int32, Int32)
bounds d blocks = (minimum ps, maximum ps)
  where
    ps = map (view $ blockXYZ . d . posValue) $ _blocks blocks

-- | Centre blocks on the origin of the supplied dimension.
centre :: Dimension -> Blocks -> Blocks
centre d blocks = move d (- (w `div` 2) - mn) blocks
  where
    w        = mx - mn
    (mn, mx) = bounds d blocks

-- | Centre blocks on the xz plane
centre_xz :: Blocks -> Blocks
centre_xz = centre x . centre z

setPositionKind :: PosKind -> Blocks -> Blocks
setPositionKind k = mapBlocks $ over blockXYZ $ \xyz ->
  case xyz of
    (XYZ x y z) -> XYZ (x & posKind .~ k) (y & posKind .~ k) (z & posKind .~ k)

-- | Repeat structure 'n' times with function 'f' applied iteratively.
repeat :: (Blocks -> Blocks) -> Int32 -> Blocks -> Blocks
repeat f n = mconcat . take (fromIntegral n) . iterate f

-- | replicate structure 'n' times with a spacing 'i' in dimension 'd'.
replicate :: Dimension -> Int32 -> Int32 -> Blocks -> Blocks
replicate d i = repeat (move d i)

-- | Substitute one block for another, intended to be used infix, e.g.
-- @ castle `subst` (cobblestone, sandstone) @
subst :: Blocks -> (Block, Block) -> Blocks
subst blocks (from, to) = mapKind f blocks
  where
    f k | k == from = to
        | otherwise = k

-- * Grid Layouts

-- | place 'Blocks' on a grid
--
-- Here we have various components like 't', 'k', and 'g'. We want them layed out in in a grid with 50 spaces between the components.
--
-- @
--    grid 50 {-spacing-}
--         [ [ t,  t,  t]
--         , [ t,  k,  t]
--         , [ t,  g,  t]
--         ]
-- @
--
grid :: Int32 -> [[Blocks]] -> Blocks
grid spacing = f z . map (f x . map centre_xz)
  where
    f :: Dimension -> [Blocks] -> Blocks
    f d = foldr (\a b -> a <> move d spacing b) mempty

-- | layout blocks on a xz plane @(y==0)@. Overhead view.
xzLayout :: [[Block]] -> Blocks
xzLayout blks = Blocks $ concat $ xzLayout' 0 blks
  where
    xzLayout' :: Int32 -> [[Block]] -> [[BlockXYZ]]
    xzLayout' _ [] = []
    xzLayout' z (r:rs) = (xLayout z (zip [0..] r)) : (xzLayout' (succ z) rs)

    xLayout :: Int32 -> [(Int32, Block)] -> [BlockXYZ]
    xLayout z ((x, blk):bs) = (BlockXYZ (xyz x 0 z) blk Nothing) : xLayout z bs

-- | layout blocks on the xy plane @(z == 0)@.
xyLayout :: [[Block]] -> Blocks
xyLayout blks = Blocks $ concat $ xyLayout' (pred (fromIntegral (length blks))) blks
  where
    xyLayout' :: Int32 -> [[Block]] -> [[BlockXYZ]]
    xyLayout' _ [] = []
    xyLayout' y (r:rs) = (xLayout y (zip [0..] r)) : (xyLayout' (pred y) rs)

    xLayout :: Int32 -> [(Int32, Block)] -> [BlockXYZ]
    xLayout y ((x, blk):bs) = (BlockXYZ (xyz x y 0) blk Nothing) : xLayout y bs

-- | layout blocks on the yz plane @(x == 0)@.
yzLayout :: [[Block]] -> Blocks
yzLayout blks = Blocks $ concat $ yzLayout' (pred (fromIntegral (length blks))) blks
  where
    yzLayout' :: Int32 -> [[Block]] -> [[BlockXYZ]]
    yzLayout' _ [] = []
    yzLayout' z (r:rs) = (yLayout z (zip [0..] r)) : (yzLayout' (pred z) rs)

    yLayout :: Int32 -> [(Int32, Block)] -> [BlockXYZ]
    yLayout z ((y, blk):bs) = (BlockXYZ (xyz 0 y z) blk Nothing) : yLayout z bs

-- * Randomise

-- | randomly replace blocks
randomise :: Double -- ^ probability that a block will be replaced
          -> (Block, Block) -- ^ (from, to) if a block is kind 'from' and selected for replacement, replace it with 'to'
          -> Blocks -- ^ 'Blocks' to (potentially) alter
          -> IO Blocks
randomise p (from, to) (Blocks bs) =
    Blocks <$> mapM f bs
  where
    f b | view blockKind b == from = do
              r <- randomIO
              return $ if r < p
                       then set blockKind to b
                       else b
        | otherwise = return b

-- | Cobblestone Blocks with a 20% chance of moss.
mossy :: Blocks -> IO Blocks
mossy = randomise 0.2 (Cobblestone, MossyCobblestone)

-- * Simple Shapes

-- | Create a line of cobblestone blocks with length 'n' along dimension 'd'.
line :: Dimension -> Int32 -> Blocks
line d n = replicate d 1 n zero # Cobblestone

-- | draw a line between 2 points
line2p :: (Int32, Int32, Int32) -> (Int32, Int32, Int32) -> Blocks
line2p (x0, y0, z0) (x1, y1, z1) =
  let dx = x1 - x0
      dy = y1 - y0
      dz = z1 - z0
      steps = maximum (map abs [dx, dy, dz])
      xInc = (fromIntegral dx) / (fromIntegral steps)
      yInc = (fromIntegral dy) / (fromIntegral steps)
      zInc = (fromIntegral dz) / (fromIntegral steps)
  in mkBlocks [ xyz (x0 + round (xInc * (fromIntegral step))) (y0 + round (yInc * (fromIntegral step))) (z0 + round (zInc * (fromIntegral step))) | step <- [0..steps] ]

-- | draw a line from the origin to the p
line1p :: (Int32, Int32, Int32) -> Blocks
line1p (x1, y1, z1) =
  let dx = x1
      dy = y1
      dz = z1
      steps = maximum (map abs [dx, dy, dz])
      xInc = (fromIntegral dx) / (fromIntegral steps)
      yInc = (fromIntegral dy) / (fromIntegral steps)
      zInc = (fromIntegral dz) / (fromIntegral steps)
  in mkBlocks [ xyz (round (xInc * (fromIntegral step))) (round (yInc * (fromIntegral step))) (round (zInc * (fromIntegral step))) | step <- [0..steps] ]

-- | convert spherical coordinates to cartesian
--
-- @(r, 0, 0)@ is a vertical line of length 'r'
--
-- @θ@ is radians of rotation from the 'y' axis
--
-- @ϕ@ is radians of rotation around the 'y' axis
spherical2cartesian :: (Double, Double, Double) -> (Int32, Int32, Int32)
spherical2cartesian (r, theta, phi) =
  let dx = round (r * sin theta * cos phi)
      dy = round (r * cos theta)
      dz = round (r * sin theta * sin phi)
  in (dx, dy, dz)

-- | cylindrical 2 cartesian (ρ, φ, z)
cylindrical2cartesian :: (Int, Double, Int32) -> (Int32, Int32, Int32)
cylindrical2cartesian (rho, phi, z) =
  let dx = round ((fromIntegral rho) * cos phi)
      dy = round ((fromIntegral rho) * sin phi)
  in (dx, dy, z)

-- | draw a catenary curve
--
-- FIXME: should not have to manually supply subdivisions
-- FIXME: the lowest part of the catenary is typically not at '0' on the y-axis. Perhaps it should be normalized?
catenary :: Int32 -- ^ width
         -> Double -- ^ 'a' - uniform scaling factor (?)
         -> Int32 -- ^ subdivisions
         -> Blocks
catenary w a sub =
  -- https://en.wikipedia.org/wiki/Catenary
  let y :: Double -> Int32
      y x = round ((a * cosh (x / a)) - a)
  in
    mkBlocks [ xyz (round x) (y x) 0 | x <-  [ (fromIntegral w') / (fromIntegral sub) | w' <- [0..(w * sub)]]]

-- | Place the specified number of blocks, evenly spaced along a
-- circle of radius 'r' on the plane 'p'
-- centered on (r,r).
circle :: Plane
       -> Int32 -- ^ radius
       -> Int32 -- ^ number of blocks
       -> Blocks
circle (Plane d d') r steps = move d r . move d' r $
    mkBlocks [ set d (ab x) . set d' (ab z) $ origin -- TODO use relative coords
             | s <- [1..steps]
             , let phi = 2*pi*fromIntegral s / fromIntegral steps :: Double
                   z   = round $ fromIntegral r * cos phi
                   x   = round $ fromIntegral r * sin phi
             ]

-- | draw a continous circle of radius r in the plane 'p'
--
-- The blocks are returned so that adjacent blocks in the list are
-- adjacent on the screen.
--
-- uses Brensenhams Circle Drawing Algorithm
circle2 :: Plane -- ^ plane
        -> Int32 -- ^ radius
        -> Blocks
circle2 (Plane d d') r =
  let octant1 = go 0 r (3 - (2 * r))
  in mkBlocks $ map mkXYZ' octant1 ++
                 (map (mkXYZ' . octant2) octant1 ++
                  (map (mkXYZ' . octant3) octant1 ++
                   (map (mkXYZ' . octant4) octant1 ++
                    (map (mkXYZ' . octant5) octant1 ++
                     (map (mkXYZ' . octant6) octant1 ++
                      (map (mkXYZ' . octant7) octant1 ++
                       (map (mkXYZ' . octant8) octant1)))))))
  where
    mkXYZ' (x,z) = set d (ab x) . set d' (ab z) $ origin

    octant2 (x,z) = (x, (-z))
    octant3 (x,z) = (z, (-x))
    octant4 (x,z) = ((-z), (-x))
    octant5 (x,z) = ((-x), (-z))
    octant6 (x,z) = ((-x), z)
    octant7 (x,z) = ((-z), x)
    octant8 (x,z) = (z, x)

    go x y d = (x, y) : step x y d

    step x y d
      | y <= x = []
      | d <  0 =
          let x' = x + 1
              d' = d + (4 * x) + 6
          in go x' y d'
      | otherwise =
          let x' = x + 1
              d' = d + (4 * (x - y)) + 10
              y' = y - 1
          in go x' y' d'

-- | A solid cylinder of radius r in the plane formed by plane 'p' and with length along dl.
solidCylinder :: Plane
              -> Dimension
              -> Int32 -- ^ radius
              -> Int32 -- ^ height
              -> Blocks
solidCylinder p dl r h = replicate dl 1 h $ solidCircle p r

-- | A hollow rectangle on a plane
--
-- positioned so that the bottom, left corner is at the origin
rectangle :: Plane -- ^ plane
          -> Int32 -- ^ width (lenth along dimension 1)
          -> Int32 -- ^ height (length along dimension 2)
          -> Blocks
rectangle (Plane d1 d2) w h =
  line d1 w <> --  top line
  move d2 (h-1) (line d1 w) <> -- bottom line
  (move d2 1 (line d2 (h - 2))) <> -- left line
  move d1 (w - 1) (move d2 1 (line d2 (h - 2))) -- right line

-- | A hollow square on the plane 'p'
square :: Plane
       -> Int32 -- ^ width
       -> Blocks
square (Plane d1 d2) w =
    l d1 <> move d2 (w-1) (l d1) <>
    l d2 <> move d1 (w-1) (l d2)
  where
    l :: Dimension -> Blocks
    l d = line d w

-- | An upright hollow cone in the (x,z) place, centered on (r,r).
cone :: Int32 -- ^ radius
     -> Int32 -- ^ height
     -> Int32 -- ^ steps
     -> Blocks
cone r h steps = mconcat
    [ translate (r - r') y (r - r') $ circle zxPlane r' steps
    | y <- [0..h]
    , let r' = round $ fromIntegral (r*(h-y)) / (fromIntegral h :: Double)
    ]

-- | An upward spiral in the (x,z) plane centered on (r,r).
spiral :: Int32 -- ^ radius
       -> Int32 -- ^ height
       -> Int32 -- ^ revolutions
       -> Int32 -- ^ steps
       -> Blocks
spiral r h revs steps = translate r 0 r $
    mkBlocks [ xyz x y z
             | s   <- [1..steps]
             , let phi = 2*pi*fromIntegral (revs*s) / fromIntegral steps :: Double
                   z   = round $ fromIntegral r * cos phi
                   x   = round $ fromIntegral r * sin phi
                   y   = round $ fromIntegral (h*s) / (fromIntegral steps :: Double)
             ]

-- | a solid, 1 brick wide rectangle
solidRectangle :: Dimension -- ^ rectangle direction (dimension)
               -> Int32 -- ^ length
               -> Int32 -- ^ height
               -> Blocks
solidRectangle d w h = replicate y 1 h $ line d w

-- * Castle Combinators

-- | a solid, 1 brick wide wall
wall :: Dimension -- ^ wall direction (dimension)
     -> Int32 -- ^ length
     -> Int32 -- ^ height
     -> Blocks
wall d w h = solidRectangle d w h

-- | a solid, 1 brick deep floor
floor :: Int32 -- ^ x-width
      -> Int32 -- ^ z-width
      -> Blocks
floor wx wz
    = replicate x 1 wx
    . replicate z 1 wz
    $ zero # Cobblestone

-- | create a square area, enclosed by 4 walls
--
-- This is also known as a rectangular prism
squareWall :: Int32 -- ^ width
           -> Int32 -- ^ height
           -> Blocks
squareWall w h = repeat (move y 1) h (square zxPlane w)

-- | A square roof of thickness 't' and width 'w'.
squareRoof :: Int32 -> Int32 -> Blocks
squareRoof t w = mconcat
    [ translate i 0 i $ square zxPlane (w-2*i)
    | i <- [0..t-1]
    ]

-- | create square Crenellations
squareCrenellations :: Int32 -- ^ width
                    -> Blocks
squareCrenellations w =
    l x <> move z (w-1) (l x) <>
    l z <> move x (w-1) (l z)
  where
    l :: Dimension -> Blocks
    l d = replicate d 2 (w `div` 2 + w `rem` 2) blk
    blk = zero # Cobblestone

-- | square turret
squareTurret :: Int32 -- ^ width
             -> Int32 -- ^ height
             -> Blocks
squareTurret w h = mconcat
    [ squareWall w h
    , translate (-1) h (-1) $ mconcat
        [ floor w' w'
        , squareWall w' 2
        , move y 2 (squareCrenellations w')
        ]
    , translate (w `div` 2) 1    0  windows
    , translate (w `div` 2) 1 (w-1) windows
    ]
  where
    w'      = w + 2
    windows = replicate y 3 (h `div` 3) zero

-- | a circular wall (with possible gaps)
--
-- This really creates @steps@ pillars with @height@ on a circus of
-- @radius@. If the number of steps is great enough, the wall will be
-- continous. Specifying only a few steps will create gaps in the
-- wall. That can be used for effect.
circleWall :: Int32 -- ^ radius
           -> Int32 -- ^ height
           -> Int32 -- ^ steps
           -> Blocks
circleWall r h steps =
    replicate y 1 h (circle zxPlane r steps)

-- | a circular wall with no gaps
circle2Wall :: Int32 -- ^ radius
            -> Int32 -- ^ height
            -> Blocks
circle2Wall r h =
    replicate y 1 h (circle2 zxPlane r)

-- | A filled circle of radius r in the plane 'p', centered on (r,r).
solidCircle :: Plane
            -> Int32 -- ^ radius of circle
            -> Blocks
solidCircle (Plane d d') r = move d r . move d' r $
    mkBlocks [ set d (ab x) . set d' (ab z) $ origin -- TODO use relative coords
             | x <- [-r..r]
             , z <- [-r..r]
             , let r' = sqrt (fromIntegral $ x*x + z*z) :: Double
             , r' <= fromIntegral r
             ]

-- | a circular floor
circleFloor :: Int32 -- ^ radius
            -> Blocks
circleFloor = solidCircle zxPlane


-- | spiral staircase
spiralStairs :: Int32 -- ^ radius
             -> Int32 -- ^ stair width (?)
             -> Int32 -- ^ height
             -> Int32 -- ^ revolutions
             -> Int32 -- ^ steps
             -> Blocks
spiralStairs r t h revs steps = mconcat
    [ translate i 0 i $ spiral (r-i) h revs steps
    | i <- [0..t-1]
    ]

-- | circular Turret
circularTurret :: Int32 -- ^ radius
               -> Int32 -- ^ height
               -> Int32 -- ^ steps
               -> Blocks
circularTurret r h steps = mconcat
    [ solidCylinder zxPlane y r h    # Air -- clear space
    , translate 1 0 1 $ spiralStairs (r-1) 3 h 3 (3*steps) -- spiral staircase
    , circleWall r h steps
    , translate (-1) h (-1) (circleFloor r' <> -- upper floor
                             circleWall r' 2 (2 * steps) <> -- upper wall
                             move y 2 (circle zxPlane r' (steps `div` 2))) -- crenellations
    , translate 2 h 2 $ floor 3 3 # Air -- exit for staircase
    , move y 1 $ repeat (move y (h `div` 3)) 3 (circle zxPlane r 4) # Air -- windows
    ]
  where r' = r + 1

-- | Germanic circular turret
circularTurret_germanic :: Int32 -- ^ radius
                        -> Int32 -- ^ height
                        -> Int32 -- ^ steps
                        -> Blocks
circularTurret_germanic r h steps =
    circularTurret r h steps <>
    translate (-1) h (-1)
         (move y 1 (circle zxPlane r' 4) # Air <> -- top windows
          move y 2 (cone r' 8 (2 * steps) # Bricks)) -- cone roof
  where r' = r + 1

--  | Make an archway of radius 'r' and thickness 't'.
archway :: Int32 -- ^ radius
        -> Int32 -- ^ thickness
        -> Blocks
archway r t
    =  solidArch r t
    <> move x 1 (solidArch (r-1) t # Air)

--  | Make a solid arch of radius 'r' and thickness 't'.
solidArch :: Int32 -- ^ radius
          -> Int32 -- ^ thickness
          -> Blocks
solidArch r t
    = replicate z 1 t
    $ solidCircle xyPlane r <> wall x (2*r + 1) r

-- | Castle Keep
castleKeep :: Blocks -- ^ structure to place at corners
           -> Int32 -- ^ width
           -> Int32 -- ^ height
           -> Blocks
castleKeep t w h = mconcat
    [ floors
    , squareWall w h
    , move y h (squareCrenellations w)
    , grid (w-1) [ [ t,  t]
                 , [ t,  t]
                 ]
    -- make a large archway that juts out
    , translate (w `div` 2) 0 (w-1) $ centre x $ archway 3 3
    ]
  where
    floors
        = translate 1 (-1) 1
        . replicate y 6 3
        $ floor w' w' # OakPlanks
    w' = w - 2

tunnel :: Int32 -> Double -> Int32 -> Int32 -> Blocks
tunnel w a sub len =
  let cat = rotate_180 xyPlane $ catenary w a sub
      (minY, maxY) = bounds y cat
  in
    replicate z 1 len (mirror x True $ move y (0 - minY) $ cat)

-- | Castle wall
--
-- A castle wall has two walls and a roof and a hallway between the walls
castleWall :: Int32 -- ^ length
           -> Int32 -- ^ height
           -> Blocks
castleWall w h = mconcat
    [ squareWall w h                             -- outer wall
    , translate 3    0  3 (squareWall (w-6) h)   -- inner wall
    , translate 1 (h-1) 1 (squareRoof 2 (w-2))   -- roof
    , translate (-1) (h-1) (-1)
        (squareWall (w+2) 2 <> move y 2 (squareCrenellations (w+2))) -- overhangs
    , translate 3 h 3 (square zxPlane (w-6) # OakFence)  -- wall top fencing
    , translate (w `div` 2) 0 (w-4) $ centre x $ archway 4 4
    ]

-- | An English Castle
englishCastle :: Blocks
englishCastle = mconcat
   [ castleWall 100{-width-} 10{-height-}
   , grid 50 {-spacing-}
       [ [ t,  t,  t]
       , [ t,  k,  t]
       , [ t,  g,  t]
       ]
   ]
 where
   t  = circularTurret 4{-radius-} 15{-height-} 20
   t' = circularTurret 3{-radius-} 15{-height-} 20
   k  = castleKeep t' 24{-width-} 15{-height-}
   g  = move x (-12) t <> move x 12 t -- gatehouse entrance has two turrets together


-- * Output Function

-- | create a minecraft function which generates specified 'Blocks'
--
-- This function will write out a @.mcfunction@ file contains all the commands.
--
-- This supplied level name should be a level which has already been created by Minecraft.
--
-- e.g.
--
-- @
--    renderFn "\/Users\/stepcut\/Library\/ApplicationSupport\/minecraft\/" "minecraft-data" "rings" (0,0,0) $ toTilda' $ repeat (move z 1 . move y 1) 100 $ circle2 x y 10
-- @
--
-- After generating the `.mcfunction` you can run it in minecraft via
--
-- @
--    /reload
--    /function haskell:rings
-- @
--
-- You can find more information on where the minecraftDir is for your system here,
--
--    <https:\/\/help.minecraft.net\/hc\/en-us\/articles\/360035131551-Where-are-Minecraft-files-stored->
--
renderFn :: FilePath -- ^ minecraftDir
         -> String   -- ^ level name
         -> String   -- ^ function name
         -> (Int32, Int32, Int32) -- ^ offset
         -> Blocks -- ^ blocks
         -> IO ()
renderFn minecraftDir levelName functionName (tx,ty,tz) (prune -> blocks) =
  do createDirectoryIfMissing True baseDir
     -- FIXME: we do not need to recreate this everytime
     withFile mcmetaPath WriteMode $ \hnd ->
       hPutStrLn hnd $ unlines $ [ "{"
                                 , " \"pack\": {"
                                 , "  \"pack_format\": 3,"
                                 , "  \"description\": \"haskell data pack\""
                                 , " }"
                                 , "}"
                                 ]
     withFile filePath WriteMode $ \hnd ->
       forM_ (_blocks $ translate tx ty tz blocks) $ \(BlockXYZ xyz blk mold) ->
        LT.hPutStrLn hnd $ B.toLazyText $ render (SetBlock xyz blk mold)
  where
    baseDir = foldr (</>) []
                [ minecraftDir, "saves", levelName, "datapacks"
                , "haskell", "data", "haskell", "functions"
                ]

    filePath = baseDir </> (functionName ++ ".mcfunction")
    mcmetaPath = foldr (</>) "pack.mcmeta" [ minecraftDir, "saves", levelName, "datapacks", "haskell" ]


-- | Removes unnecessary setblock instructions from the list.
prune :: Blocks -> Blocks
prune = Blocks . Map.elems . Map.fromList . map (_blockXYZ &&& id) . _blocks
