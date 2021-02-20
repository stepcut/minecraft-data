module Main where

import Control.Exception (bracket)
import Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)
import qualified Data.Map as Map
import qualified Data.Vector as Vector
import Minecraft.Anvil (ChunkX, ChunkZ, ChunkData, ChunkMap, compressChunkData, writeChunkMap)
import Minecraft.Core (BlockId(..), toNBT)
import Minecraft.Chunk (Chunk(..), Section(..), emptyChunk, emptySection)
import System.IO (IOMode(WriteMode), withFile)

section0 :: Section
section0 =
  emptySection { _Blocks = Vector.replicate 4096 (BlockId 20)
               }

chunk0 :: Chunk
chunk0 = emptyChunk { _Sections = [section0] }

chunkData0 :: ChunkData
chunkData0 = compressChunkData (toNBT chunk0)

chunkMap :: POSIXTime -> ChunkMap
chunkMap now = Map.fromList
  [ ((0,0), (chunkData0, now)) ]

main :: IO ()
main =
  withFile "test-r.0.0.mca" WriteMode $ \h ->
    do now <- getPOSIXTime
       writeChunkMap h (chunkMap now)
