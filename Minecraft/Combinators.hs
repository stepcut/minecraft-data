{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
-- http://www.timphilipwilliams.com/posts/2019-07-25-minecraft.html
module Minecraft.Combinators where

import           Prelude hiding (repeat, replicate, floor)
import           Control.Arrow
import           Control.Lens (Lens', (^.), (&), (.~), view, over, set, makeLenses)
import           Control.Monad
import           Data.Int (Int32)
import qualified Data.Text.Lazy.IO as LT
import qualified Data.Text.Lazy.Builder as B
import qualified Data.Map as Map
import           Minecraft.Block
import           Minecraft.Command (Command(SetBlock), OldBlockHandling)
import           Minecraft.Core (Pos(..), PosKind(..), XYZ(..), xyz, x, y ,z, origin, posKind, posValue, render, ab)
import           System.Random (randomIO)
import           System.FilePath
import           System.IO

data BlockXYZ = BlockXYZ
  { _blockXYZ  :: XYZ
  , _blockKind :: Block
  , _blockOldHandling :: Maybe OldBlockHandling
  }
  deriving (Eq, Show)
makeLenses ''BlockXYZ

newtype Blocks = Blocks { unBlocks :: [BlockXYZ] }
    deriving (Semigroup, Monoid, Show)

-- | make Cobblestone blocks at the supplied XYZ coordinates
mkBlocks :: [XYZ] -> Blocks
mkBlocks = Blocks . map (\c -> BlockXYZ c Cobblestone Nothing)

-- | A block of nothing (air) at the origin (0,0,0)
zero :: Blocks
zero = Blocks [BlockXYZ origin Air Nothing]

-- | map a function `(BlockXYZ -> BlockXYZ)`
mapBlocks :: (BlockXYZ -> BlockXYZ) -> Blocks -> Blocks
mapBlocks f = Blocks . map f . unBlocks

-- | map a function `(Block -> Block)`
mapKind :: (Block -> Block) -> Blocks -> Blocks
mapKind f = mapBlocks $ over blockKind f

-- | Set the kind of all blocks
infixr 8 #
(#) :: Blocks -> Block -> Blocks
(#) blocks k = mapKind (const k) blocks

-- | We will abstract over dimensions using lenses:
type Dimension = Lens' XYZ Pos

-- | Move blocks by 'i' in dimension 'd'.
move :: Dimension -> Int32 -> Blocks -> Blocks
move d i = mapBlocks $ over (blockXYZ . d . posValue) (+i)

-- | Translate blocks by the supplied 'x, y, z' offset.
translate :: Int32 -> Int32 -> Int32 -> Blocks -> Blocks
translate x' y' z' = move x x' . move y y' . move z z'

-- FIXME: does this do something sensible with tilda/caret notation?
rotate_x, rotate_y :: Blocks -> Blocks
rotate_x = mapBlocks $ over blockXYZ $ \xyz ->
             case xyz of
               (XYZ x y z)  -> XYZ x z y
rotate_y = mapBlocks $ over blockXYZ $ \xyz ->
             case xyz of
               (XYZ x y z) -> XYZ y x z

-- | Get the coordinate bounds for blocks along a particular dimension 'd'.
bounds :: Dimension -> Blocks -> (Int32, Int32)
bounds d blocks = (minimum ps, maximum ps)
  where
    ps = map (view $ blockXYZ . d . posValue) $ unBlocks blocks

-- | Centre blocks on the origin of the supplied dimension.
centre :: Dimension -> Blocks -> Blocks
centre d blocks = move d (- (w `div` 2) - mn) blocks
  where
    w        = mx - mn
    (mn, mx) = bounds d blocks

centre_xz :: Blocks -> Blocks
centre_xz = centre x . centre z

grid :: Int32 -> [[Blocks]] -> Blocks
grid spacing = f z . map (f x . map centre_xz)
  where
    f :: Dimension -> [Blocks] -> Blocks
    f d = foldr (\a b -> a <> move d spacing b) mempty

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

randomise :: Double -> (Block, Block) -> Blocks -> IO Blocks
randomise p (from, to) (Blocks bs) =
    Blocks <$> mapM f bs
  where
    f b | view blockKind b == from = do
              r <- randomIO
              return $ if r < p
                       then set blockKind to b
                       else b
        | otherwise = return b

-- | Create a line of cobblestone blocks with length 'n' along dimension 'd'.
line :: Dimension -> Int32 -> Blocks
line d n = replicate d 1 n zero # Cobblestone

-- | A circle of radius r in the plane formed by dimensions (d, d'), centered on (r,r).
circle :: Dimension -> Dimension -> Int32 -> Int32 -> Blocks
circle d d' r steps = move d r . move d' r $
    mkBlocks [ set d (ab x) . set d' (ab z) $ origin -- TODO use relative coords
             | s <- [1..steps]
             , let phi = 2*pi*fromIntegral s / fromIntegral steps :: Double
                   z   = round $ fromIntegral r * cos phi
                   x   = round $ fromIntegral r * sin phi
             ]


-- | A solid cylinder of radius r in the plane formed by dimensions (d, d') and with length along dl.
solidCylinder :: Dimension -> Dimension -> Dimension -> Int32 -> Int32 -> Blocks
solidCylinder d d' dl r h = replicate dl 1 h $ solidCircle d d' r

wall :: Dimension -> Int32 -> Int32 -> Blocks
wall d w h = replicate y 1 h $ line d w

floor :: Int32 -> Int32 -> Blocks
floor wx wz
    = replicate x 1 wx
    . replicate z 1 wz
    $ zero # Cobblestone

square :: Int32 -> Blocks
square w =
    l x <> move z (w-1) (l x) <>
    l z <> move x (w-1) (l z)
  where
    l :: Dimension -> Blocks
    l d = line d w


squareWall :: Int32 -> Int32 -> Blocks
squareWall w h = repeat (move y 1) h (square w)

-- | A square roof of thickness 't' and width 'w'.
squareRoof :: Int32 -> Int32 -> Blocks
squareRoof t w = mconcat
    [ translate i 0 i $ square (w-2*i)
    | i <- [0..t-1]
    ]

squareCrenellations :: Int32 -> Blocks
squareCrenellations w =
    l x <> move z (w-1) (l x) <>
    l z <> move x (w-1) (l z)
  where
    l :: Dimension -> Blocks
    l d = replicate d 2 (w `div` 2 + w `rem` 2) blk
    blk = zero # Cobblestone

squareTurret :: Int32 -> Int32 -> Blocks
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

circleWall :: Int32 -> Int32 -> Int32 -> Blocks
circleWall r h steps =
    replicate y 1 h (circle x z r steps)

-- | A filled circle of radius r in the plane formed by dimensions (d, d'), centered on (r,r).
solidCircle :: Dimension -> Dimension -> Int32 -> Blocks
solidCircle d d' r = move d r . move d' r $
    mkBlocks [ set d (ab x) . set d' (ab z) $ origin -- TODO use relative coords
             | x <- [-r..r]
             , z <- [-r..r]
             , let r' = sqrt (fromIntegral $ x*x + z*z) :: Double
             , r' <= fromIntegral r
             ]

circleFloor :: Int32 -> Blocks
circleFloor = solidCircle x z

-- | An upward spiral in the (x,z) plane centered on (r,r).
spiral :: Int32 -> Int32 -> Int32 -> Int32 -> Blocks
spiral r h revs steps = translate r 0 r $
    mkBlocks [ xyz x y z
             | s   <- [1..steps]
             , let phi = 2*pi*fromIntegral (revs*s) / fromIntegral steps :: Double
                   z   = round $ fromIntegral r * cos phi
                   x   = round $ fromIntegral r * sin phi
                   y   = round $ fromIntegral (h*s) / (fromIntegral steps :: Double)
             ]

spiralStairs :: Int32 -> Int32 -> Int32 -> Int32 -> Int32 -> Blocks
spiralStairs r t h revs steps = mconcat
    [ translate i 0 i $ spiral (r-i) h revs steps
    | i <- [0..t-1]
    ]

circularTurret :: Int32 -> Int32 -> Int32 -> Blocks
circularTurret r h steps = mconcat
    [ solidCylinder x z y r h    # Air -- clear space
    , translate 1 0 1 $ spiralStairs (r-1) 3 h 3 (3*steps) -- spiral staircase
    , circleWall r h steps
    , translate (-1) h (-1) (circleFloor r' <> -- upper floor
                             circleWall r' 2 (2 * steps) <> -- upper wall
                             move y 2 (circle x z r' (steps `div` 2))) -- crenellations
    , translate 2 h 2 $ floor 3 3 # Air -- exit for staircase
    , move y 1 $ repeat (move y (h `div` 3)) 3 (circle x z r 4) # Air -- windows
    ]
  where r' = r + 1


-- | An upright hollow cone in the (x,z) place, centered on (r,r).
cone :: Int32 -> Int32 -> Int32 -> Blocks
cone r h steps = mconcat
    [ translate (r - r') y (r - r') $ circle x z r' steps
    | y <- [0..h]
    , let r' = round $ fromIntegral (r*(h-y)) / (fromIntegral h :: Double)
    ]


circularTurret_germanic :: Int32 -> Int32 -> Int32 -> Blocks
circularTurret_germanic r h steps =
    circularTurret r h steps <>
    translate (-1) h (-1)
         (move y 1 (circle x z r' 4) # Air <> -- top windows
          move y 2 (cone r' 8 (2 * steps) # Bricks)) -- cone roof
  where r' = r + 1

--  | Make an archway of radius 'r' and thickness 't'.
archway :: Int32 -> Int32 -> Blocks
archway r t
    =  solidArch r t
    <> move x 1 (solidArch (r-1) t # Air)

--  | Make a solid arch of radius 'r' and thickness 't'.
solidArch :: Int32 -> Int32 -> Blocks
solidArch r t
    = replicate z 1 t
    $ solidCircle x y r <> wall x (2*r + 1) r

castleKeep :: Blocks -> Int32 -> Int32 -> Blocks
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

castleWall :: Int32 -> Int32 -> Blocks
castleWall w h = mconcat
    [ squareWall w h                             -- outer wall
    , translate 3    0  3 (squareWall (w-6) h)   -- inner wall
    , translate 1 (h-1) 1 (squareRoof 2 (w-2))   -- roof
    , translate (-1) (h-1) (-1)
        (squareWall (w+2) 2 <> move y 2 (squareCrenellations (w+2))) -- overhangs
    , translate 3 h 3 (square (w-6) # OakFence)  -- wall top fencing
    , translate (w `div` 2) 0 (w-4) $ centre x $ archway 4 4
    ]

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

mossy :: Blocks -> IO Blocks
mossy = randomise 0.2 (Cobblestone, MossyCobblestone)

-- | Removes unnecessary setblock instructions from the list.
prune :: Blocks -> Blocks
prune = Blocks . Map.elems . Map.fromList . map (_blockXYZ &&& id) . unBlocks

renderFn :: FilePath -> String -> String -> (Int32, Int32, Int32) -> Blocks -> IO ()
renderFn minecraftDir levelName functionName (tx,ty,tz) (prune -> blocks) =
    withFile filePath WriteMode $ \hnd ->
        forM_ (unBlocks $ translate tx ty tz blocks) $ \(BlockXYZ xyz blk mold) ->
          LT.hPutStrLn hnd $ B.toLazyText $ render (SetBlock xyz blk mold)
  where
    filePath = foldr (</>) (functionName ++ ".mcfunction")
                   [ minecraftDir, "saves", levelName, "datapacks"
                   , "haskell", "data", "haskell",  "functions" ]
