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
import           Control.Lens (Lens', view, over, set, makeLenses)
import           Control.Monad
import           Data.Int (Int32)
import qualified Data.Text.Lazy.IO as LT
import qualified Data.Text.Lazy.Builder as B
import qualified Data.Map as Map
import           Minecraft.Block
import           Minecraft.Command (Command(SetBlock), OldBlockHandling)
import           Minecraft.Core (XYZ(..), x, y ,z, render)
import           System.Random
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
zero = Blocks [BlockXYZ (XYZ 0 0 0) Air Nothing]

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
type Dimension = Lens' XYZ Int32

-- | Move blocks by 'i' in dimension 'd'.
move :: Dimension -> Int32 -> Blocks -> Blocks
move d i = mapBlocks $ over (blockXYZ . d) (+i)

-- | Translate blocks by the supplied 'x, y, z' offset.
translate :: Int32 -> Int32 -> Int32 -> Blocks -> Blocks
translate x' y' z' = move x x' . move y y' . move z z'

rotate_x, rotate_y :: Blocks -> Blocks
rotate_x = mapBlocks $ over blockXYZ $ \xyz ->
             case xyz of
               (XYZ x y z)  -> XYZ x z y
               (RXYZ x y z) -> RXYZ x z y
rotate_y = mapBlocks $ over blockXYZ $ \xyz ->
             case xyz of
               (XYZ x y z) -> XYZ y x z
               (RXYZ x y z) -> RXYZ y x z

makeRelative :: Blocks -> Blocks
makeRelative = mapBlocks $ over blockXYZ $ \xyz ->
  case xyz of
    (XYZ x y z) -> RXYZ x y z
    _ -> xyz

-- | Get the coordinate bounds for blocks along a particular dimension 'd'.
bounds :: Dimension -> Blocks -> (Int32, Int32)
bounds d blocks = (minimum ps, maximum ps)
  where
    ps = map (view $ blockXYZ . d) $ unBlocks blocks

-- | Centre blocks on the origin of the supplied dimension.
centre :: Dimension -> Blocks -> Blocks
centre d blocks = move d (- (w `div` 2) - mn) blocks
  where
    w        = mx - mn
    (mn, mx) = bounds d blocks

centre_xz :: Blocks -> Blocks
centre_xz = centre x . centre z

-- | Repeat structure 'n' times with function 'f' applied iteratively.
repeat :: (Blocks -> Blocks) -> Int32 -> Blocks -> Blocks
repeat f n = mconcat . take (fromIntegral n) . iterate f

-- | replicate structure 'n' times with a spacing 'i' in dimension 'd'.
replicate :: Dimension -> Int32 -> Int32 -> Blocks -> Blocks
replicate d i = repeat (move d i)

-- | Create a line of cobblestone blocks with length 'n' along dimension 'd'.
line :: Dimension -> Int32 -> Blocks
line d n = replicate d 1 n zero # Cobblestone

wall :: Dimension -> Int32 -> Int32 -> Blocks
wall d w h
    = replicate y 1 h
    . replicate d 1 w
    $ zero # Cobblestone


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

-- | A square of thickness 't' and width 'w'.
wideSquare :: Int32 -> Int32 -> Blocks
wideSquare t w = mconcat
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


circle :: Int32 -> Int32 -> Blocks
circle r steps = translate r 0 r $
    mkBlocks [ XYZ x 0 z
             | s <- [1..steps]
             , let phi = (2*pi*fromIntegral s) / fromIntegral steps :: Double
                   z   = round $ fromIntegral r * cos phi
                   x   = round $ fromIntegral r * sin phi
             ]

circleWall :: Int32 -> Int32 -> Int32 -> Blocks
circleWall r h steps =
    repeat (move y 1) h (circle r steps)

circleFloor :: Int32 -> Blocks
circleFloor r = translate r 0 r $
    mkBlocks [ XYZ x 0 z
             | x <- [-r..r]
             , z <- [-r..r]
             , let d = sqrt (fromIntegral $ x*x + z*z) :: Double
             , d <= fromIntegral r
             ]

cylinder :: Int32 -> Int32 -> Blocks
cylinder r h = repeat (move y 1) h (circleFloor r)

spiral :: Int32 -> Int32 -> Int32 -> Int32 -> Blocks
spiral r h revs steps = translate r 0 r $
    mkBlocks [ XYZ x y z
             | s   <- [1..steps]
             , let phi = (2*pi*fromIntegral (revs*s)) / fromIntegral steps :: Double
                   z   = round $ fromIntegral r * cos phi
                   x   = round $ fromIntegral r * sin phi
                   y   = round $ fromIntegral (h*s) / (fromIntegral steps :: Double)
             ]

wideSpiral :: Int32 -> Int32 -> Int32 -> Int32 -> Int32 -> Blocks
wideSpiral r t h revs steps = mconcat
    [ translate i 0 i $ spiral (r-i) h revs steps
    | i <- [0..t-1]
    ]

circularTurret :: Int32 -> Int32 -> Int32 -> Blocks
circularTurret r h steps = mconcat
    [ cylinder r h    # Air -- clear space
    , translate 1 0 1 $ wideSpiral (r-1) 3 h 3 (3*steps) -- spiral staircase
    , circleWall r h steps
    , translate (-1) h (-1) (circleFloor r' <> -- upper floor
                             circleWall r' 2 (2 * steps) <> -- upper wall
                             move y 2 (circle r' (steps `div` 2))) -- crenellations
    , translate 2 h 2 $ floor 3 3 # Air -- exit for staircase
    , move y 1 $ repeat (move y (h `div` 3)) 3 (circle r 4) # Air -- windows
    ]
  where r' = r + 1


cone :: Int32 -> Int32 -> Int32 -> Blocks
cone r h steps = mconcat
    [ translate (r - r') y (r - r') $ circle r' steps
    | y <- [0..h]
    , let r' = round $ fromIntegral (r*(h-y)) / (fromIntegral h :: Double)
    ]


circularTurret_germanic :: Int32 -> Int32 -> Int32 -> Blocks
circularTurret_germanic r h steps =
    circularTurret r h steps <>
    translate (-1) h (-1)
         (move y 1 (circle r' 4) # Air <> -- top windows
          move y 2 (cone r' 8 (2 * steps) # Bricks)) -- cone roof
  where r' = r + 1


grid :: Int32 -> [[Blocks]] -> Blocks
grid spacing = f z . map (f x . map centre_xz)
  where
    f :: Dimension -> [Blocks] -> Blocks
    f d = foldr (\a b -> a <> move d spacing b) mempty

-- | Make a solid archway of radius 'r' and thickness 't'.
archway :: Int32 -> Int32 -> Blocks
archway r t
    = replicate z 1 t
    $ rotate_x (circleFloor r <> floor (2*r + 1) r)

castleKeep :: Blocks -> Int32 -> Int32 -> Blocks
castleKeep t w h = mconcat
    [ floors
    , squareWall w h
    , move y h (squareCrenellations w)
    , grid (w-1) [ [ t,  t]
                 , [ t,  t]
                 ]
    -- make a larger archway from default stone that juts out,
    -- before overlaying the smaller empty space one.
    , translate (w `div` 2) 0 w $ centre x $ archway 3 2
    , translate (w `div` 2) 0 w $ centre x $ archway 2 3 # Air
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
    , translate 1 (h-1) 1 (wideSquare 2 (w-2))   -- roof
    , translate (-1) (h-1) (-1)
        (squareWall (w+2) 2 <> move y 2 (squareCrenellations (w+2))) -- overhangs
    , translate 3 h 3 (square (w-6) # OakFence)  -- wall top fencing
    -- since the wall is hollow we make a larger archway section
    -- out of default stone before we overlay the smaller empty space one
    , translate (w `div` 2) 0 (w-3) $ centre x $ archway 5 2
    , translate (w `div` 2) 0 (w-4) $ centre x $ archway 4 4 # Air
    ]

englishCastle :: Blocks
englishCastle = mconcat
    [ castleWall w h
    , grid (w `div` 2)
        [ [ t,  t,  t]
        , [ t,  k,  t]
        , [ t,  g,  t]
        ]
    ]
  where
    t  = circularTurret 4 15 20
    k  = castleKeep (circularTurret 3 15 20) kw kh
    w  = 100 -- castle
    h  = 10  -- wall height
    kw = 24  -- keep width
    kh = 15  -- keep height
    -- gatehouse entrance has two turrets together
    g  = move x (-12) t <> move x 12 t

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

mossy :: Blocks -> IO Blocks
mossy = randomise 0.2 (Cobblestone, MossyCobblestone)

-- | Substitute one block for another, intended to be used infix, e.g.
-- @ castle `subst` (cobblestone, sandstone) @
subst :: Blocks -> (Block, Block) -> Blocks
subst blocks (from, to) = mapKind f blocks
  where
    f k | k == from = to
        | otherwise = k

-- | Removes unnecessary setblock instructions from the list.
prune :: Blocks -> Blocks
prune = Blocks . Map.elems . Map.fromList . map (_blockXYZ &&& id) . unBlocks


renderLvl :: FilePath -> String -> String -> XYZ -> Blocks -> IO ()
renderLvl minecraftDir levelName functionName XYZ{..} (prune -> blocks) =
    withFile filePath WriteMode $ \hnd ->
        forM_ (unBlocks $ translate _x _y _z blocks) $ \(BlockXYZ xyz blk mold) ->
          LT.hPutStrLn hnd $ B.toLazyText $ render (SetBlock xyz blk mold)
  {-
          hPutStrLn hnd $ printf "setblock %s %s %s %s %s"
              (show _x) (show _y) (show _z) kind (maybe "" (\s -> "["++s++"]") mstate)
-}
  where
    filePath = foldr (</>) (functionName ++ ".mcfunction")
                   [ minecraftDir, "saves", levelName, "datapacks"
                   , "haskell", "data", "haskell",  "functions" ]

