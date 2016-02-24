{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Minecraft.Anvil where

import Control.Monad (replicateM)
import Codec.Compression.Zlib (decompress)
import Data.Bits (shiftR, shiftL, (.&.), (.|.))
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LB
import Data.Data (Data, Typeable)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.NBT (NBT)
import Data.Serialize (Serialize(..), Get, Put, decodeLazy, getLazyByteString, getWord8, getWord32be, putLazyByteString, putWord8, putWord32be)
import Data.Time.Clock.POSIX (POSIXTime)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Data.Word  (Word8, Word32)
import GHC.Generics
import Pipes.ByteString (fromHandle)
import Pipes.Cereal  (decodeGet)
import Pipes.Parse (runStateT)
import System.IO (Handle, hSeek, SeekMode(AbsoluteSeek))

{-
regionX :: Double -- ^ chunkX
        -> Int
regionX x = floor (x / 32.0)

regionY :: Double -- ^ chunkY
        -> Int
regionY y = foor (y / 32.0)
-}

type ChunkX = Int
type ChunkY = Int
type RegionX = Int
type RegionY = Int

regionX :: ChunkX
        -> RegionX
regionX x = x `shiftR` 5

regionY :: ChunkY
        -> RegionY
regionY y = y `shiftR` 5

chunkIndex :: ChunkX
           -> ChunkY
           -> Int
chunkIndex x z = (x `pmod` 32) + ((z `pmod` 32) * 32)
  where
    pmod n m =
      let n' = n `mod` m
      in if (n' >= 0)
         then n'
         else n' + m

type Word24 = Word32

-- | ChunkLocation
--
-- ChunkLocation 0 0 means chunk is not present in the file
data ChunkLocation = ChunkLocation
  { chunkOffset :: Word24 -- ^ number of 4KiB sectors from start of file
  , chunkLength :: Word8 -- ^ length of chunk -- units are 4KiB sectors, rounded up
  }
  deriving (Eq, Ord, Read, Show, Data, Typeable, Generic)

-- | ChunkLocation 0 0 means chunk is not present in the file
emptyChunkLocation :: ChunkLocation
emptyChunkLocation = ChunkLocation 0 0

putChunkLocation :: ChunkLocation -> Put
putChunkLocation (ChunkLocation offset len) =
  do putWord8 (fromIntegral ((offset `shiftR` 16) .&. 0xFF))
     putWord8 (fromIntegral ((offset `shiftR` 8) .&. 0xFF))
     putWord8 (fromIntegral (offset .&. 0xFF))
     putWord8 len

getChunkLocation :: Get ChunkLocation
getChunkLocation =
  do b2  <- fromIntegral <$> getWord8
     b1  <- fromIntegral <$> getWord8
     b0  <- fromIntegral <$> getWord8
     len <- fromIntegral <$> getWord8
     pure (ChunkLocation ((b2 `shiftL` 16) .|. (b1 `shiftL` 8) .|. b0) len)

instance Serialize ChunkLocation where
  put = putChunkLocation
  get = getChunkLocation

data AnvilHeader = AnvilHeader
  { locations  :: Vector ChunkLocation
  , timestamps :: Vector POSIXTime
  }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

-- guessing on the timestamp format a bit
putTimestamp :: POSIXTime -> Put
putTimestamp t = putWord32be (round t)

getTimestamp :: Get POSIXTime
getTimestamp =
  do l <- getWord32be
     pure $ (realToFrac l)

putAnvilHeader :: AnvilHeader -> Put
putAnvilHeader (AnvilHeader locations timestamps)
  | (Vector.length locations /= 1024) || (Vector.length timestamps /= 1024) =
    error "putAnvilHeader: locations and timestamps fields must be exactly 1024 entries long."
  | otherwise =
      do mapM_ putChunkLocation (Vector.toList locations)
         mapM_ putTimestamp (Vector.toList timestamps)

getAnvilHeader :: Get AnvilHeader
getAnvilHeader =
  do locations  <- replicateM 1024 getChunkLocation
     timestamps <- replicateM 1024 getTimestamp
     pure $ AnvilHeader (Vector.fromList locations) (Vector.fromList timestamps)

instance Serialize AnvilHeader where
  put = putAnvilHeader
  get = getAnvilHeader

emptyAnvilHeader :: AnvilHeader
emptyAnvilHeader =
  AnvilHeader { locations  = Vector.replicate 1024 emptyChunkLocation
              , timestamps = Vector.replicate 1024 0
              }

data CompressionType
  = GZip -- ^ unused in practice
  | Zlib
  deriving (Eq, Ord, Read, Show, Data, Typeable, Generic)

-- | ChunkData
--
-- Compressed chunk data
data ChunkData = ChunkData
  { chunkDataLength      :: Word32 -- ^ length of chunkData + 1
  , chunkDataCompression :: CompressionType -- ^ compression type
  , chunkData            :: ByteString -- ^ compressed data (length - 1)
  }
  deriving (Eq, Ord, Read, Show, Data, Typeable, Generic)

putChunkData :: ChunkData -> Put
putChunkData cd =
  do putWord32be (chunkDataLength cd)
     case chunkDataCompression cd of
       GZip -> putWord8 1
       Zlib -> putWord8 2
     putLazyByteString (chunkData cd)

getChunkData :: Get ChunkData
getChunkData =
  do len  <- getWord32be
     comp <- do w <- getWord8
                case w of
                  1 -> pure GZip
                  2 -> pure Zlib
                  _ -> error $ "Unknown compression code in getChunkData: " ++ show w
     bs   <- getLazyByteString (fromIntegral (len - 1))
     pure $ ChunkData len comp bs

instance Serialize ChunkData where
  put = putChunkData
  get = getChunkData

-- | read 'ChunkData' from a Seekable 'Handle'
readChunkData :: Handle -> ChunkLocation -> IO (Maybe ChunkData)
readChunkData h chunkLocation
  | chunkLocation == emptyChunkLocation = pure Nothing
  | otherwise =
      do hSeek h AbsoluteSeek (fromIntegral $ ((chunkOffset chunkLocation) * 4096))
         (r, _) <- runStateT (decodeGet getChunkData) (fromHandle h)
         case r of
           (Left err) -> error err
           (Right cd) -> pure (Just cd)

decompressChunkData :: ChunkData -> Either String NBT
decompressChunkData cd
  | chunkDataCompression cd == Zlib =
    decodeLazy (decompress (chunkData cd))
  | otherwise = error $ "decompressChunkData not implemented for " ++ show (chunkDataCompression cd)
