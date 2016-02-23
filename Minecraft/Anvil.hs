{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Minecraft.Anvil where

import Control.Monad (replicateM)
import Data.Bits (shiftR, shiftL, (.&.), (.|.))
import Data.Data (Data, Typeable)
import Data.Serialize (Serialize(..), Get, Put, getWord8, getWord32be, putWord8, putWord32be)
import Data.Time.Clock.POSIX (POSIXTime)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Data.Word  (Word8, Word32)
import GHC.Generics
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

emptyAnvilHeader :: AnvilHeader
emptyAnvilHeader =
  AnvilHeader { locations  = Vector.replicate 1024 emptyChunkLocation
              , timestamps = Vector.replicate 1024 0
              }
