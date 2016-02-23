module Main where

import Control.Exception (bracket)
import Data.ByteString (hGetSome)
import Data.Serialize (decode)
import Minecraft.Anvil (AnvilHeader, getAnvilHeader)
import System.IO (openFile, hClose, IOMode(ReadMode))
import System.Environment (getArgs)

main :: IO ()
main =
  do [fp] <- getArgs
     bracket (openFile fp ReadMode) hClose $ \h -> do
       bs <- hGetSome h 8192 -- ^ header size is a fixed 8KiB
       case decode bs of
         (Left err) -> putStrLn err
         (Right a)  -> print (a :: AnvilHeader)


