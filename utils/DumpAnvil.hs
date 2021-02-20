module Main where

import Control.Exception (bracket)
import Data.ByteString (hGetSome)
import Data.Serialize (decode)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Minecraft.Anvil (AnvilHeader(..), ChunkLocation(..), getAnvilHeader, readChunkData, decompressChunkData, showAnvilHeader)
import System.IO (Handle, openFile, hClose, IOMode(ReadMode))
import System.Environment (getArgs)

main :: IO ()
main =
  do [fp] <- getArgs
     bracket (openFile fp ReadMode) hClose $ \h -> do
       bs <- hGetSome h 8192 -- ^ header size is a fixed 8KiB
       case decode bs of
         (Left err) -> putStrLn err
         (Right ah)  ->
           do putStrLn (showAnvilHeader ah)
--              bs <- hGetSome h 1024
--              print bs
              dumpChunks h ah

dumpChunk :: Handle -> ChunkLocation -> IO ()
dumpChunk h chunkLocation =
  do ecd <- readChunkData h chunkLocation
     case ecd of
       (Left e) -> putStrLn e
       (Right cd) -> print (decompressChunkData cd)

dumpChunks :: Handle -> AnvilHeader -> IO ()
dumpChunks h ah =
  do mapM_ (dumpChunk h) (locations ah)



