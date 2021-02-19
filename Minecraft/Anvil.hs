{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Minecraft.Anvil where

import Control.Monad (when, replicateM)
import Control.Monad.State.Strict (execStateT, modify')
import Codec.Compression.Zlib (decompress, compress)
import Data.Bits (shiftR, shiftL, (.&.), (.|.))
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as LB
import Data.Data (Data, Typeable)
import Data.List (mapAccumL)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.NBT (NBT)
import Data.Serialize (Serialize(..), Get, Put, decodeLazy, encodeLazy, getLazyByteString, getWord8, getWord32be, putLazyByteString, putWord8, putWord32be)
import Data.Time.Clock.POSIX (POSIXTime)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Data.Word  (Word8, Word32)
import GHC.Generics
import Pipes.ByteString (fromHandle, toHandle)
import Pipes.Cereal  (decodeGet, encodePut)
import Pipes.Parse (runStateT)
import Pipes (Pipe, runEffect, (>->), await, each, yield)
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
type ChunkZ = Int
type RegionX = Int
type RegionZ = Int

type ChunkMap = Map (ChunkX, ChunkZ) (ChunkData, POSIXTime)

regionX :: ChunkX
        -> RegionX
regionX x = x `shiftR` 5

regionZ :: ChunkZ
        -> RegionZ
regionZ z = z `shiftR` 5

chunkIndex :: (ChunkX, ChunkZ)
           -> Int
chunkIndex (x, z) = (x `pmod` 32) + ((z `pmod` 32) * 32)
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

showAnvilHeader :: AnvilHeader -> String
showAnvilHeader ah = show $ AnvilHeader { locations = Vector.filter (/= emptyChunkLocation) (locations ah)
                                        , timestamps = Vector.filter (/= 0) (timestamps ah)
                                        }

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

-- | make an 'AnvilHeader'
--
-- assumes the chunks will be written in the same order as they appear in the list
mkAnvilHeader :: [((ChunkX, ChunkZ), (ChunkData, POSIXTime))]
                 -> AnvilHeader
mkAnvilHeader chunks =
  let timestamps' = map (\(i, (_, t)) -> (chunkIndex i, t)) chunks
      locations'   = snd $ mapAccumL mkLocation 0x2 chunks --  first chunk is at sector 0x2, after the AnvilHeader
  in AnvilHeader { locations  = (locations emptyAnvilHeader) Vector.// locations'
                 , timestamps = (timestamps emptyAnvilHeader) Vector.// timestamps'
                 }
  where
    mkLocation :: Word24 -> ((ChunkX, ChunkZ), (ChunkData, POSIXTime)) -> (Word24, (Int, ChunkLocation))
    mkLocation offset (chunkPos, (chunkData, _)) =
      let paddedSectorLength = ((chunkDataLength chunkData) + 4 + 4095) `div` 4096
          offset' = offset + paddedSectorLength
      in (offset', (chunkIndex chunkPos, ChunkLocation offset (fromIntegral paddedSectorLength)))

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
                  _ -> error $ "Unknown compression code in getChunkData: " ++ show (len, w)
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

-- | write 'ChunkData' to a Seekable 'Handle'
--
-- FIXME: chunks are supposed to be guaranteed less than 1MB
{-
writeChunkData :: Handle -> ChunkData -> IO ()
writeChunkData h chunkData =
  do runEffect $ (encodePut (putChunkData chunkData)) >-> (toHandle h)
     let padding = 4096 - ((chunkDataLength chunkData + 4) `mod` 4096)
     when (padding > 0) (B.hPutStr h (B.replicate (fromIntegral padding) 0))
-}

writeChunkData :: (Monad m) => Pipe ChunkData B.ByteString m ()
writeChunkData  = do
  chunkData <- await
  encodePut (putChunkData chunkData)
  let padding = 4096 - ((chunkDataLength chunkData + 4) `mod` 4096)
  when (padding > 0) (yield (B.replicate (fromIntegral padding) 0))

{-

  do bytes <- execStateT (runEffect $ (encodePut (putChunkData chunkData)) >-> counter >-> (toHandle h)) 0
     pure bytes
       where
         counter =
           do bs <- await
              modify' $ \i -> i + B.length bs
              yield bs
-}
{-
readChunkData :: Handle -> ChunkLocation -> IO (Maybe ChunkData)
readChunkData h chunkLocation
  | chunkLocation == emptyChunkLocation = pure Nothing
  | otherwise =
      do hSeek h AbsoluteSeek (fromIntegral $ ((chunkOffset chunkLocation) * 4096))
         (r, _) <- runStateT (decodeGet getChunkData) (fromHandle h)
         case r of
           (Left err) -> error err
           (Right cd) -> pure (Just cd)
-}
decompressChunkData :: ChunkData -> Either String NBT
decompressChunkData cd
  | chunkDataCompression cd == Zlib =
    decodeLazy (decompress (chunkData cd))
  | otherwise = error $ "decompressChunkData not implemented for " ++ show (chunkDataCompression cd)

-- | NBT needs to be a Chunk
compressChunkData :: NBT -> ChunkData
compressChunkData nbt =
  let d = compress (encodeLazy nbt)
  in ChunkData { chunkDataLength      = 1 + fromIntegral (LB.length d)
               , chunkDataCompression = Zlib
               , chunkData            = d
               }

writeChunkMap :: Handle -> ChunkMap -> IO ()
writeChunkMap h chunkMap =
  do hSeek h AbsoluteSeek 0
     let chunks = Map.toAscList chunkMap
     runEffect $ (encodePut (putAnvilHeader (mkAnvilHeader chunks))) >-> (toHandle h)
     runEffect $ (each $ map (fst . snd) chunks) >-> writeChunkData >-> (toHandle h)
--     runEffect $ (encodePut (putAnvilHeader emptyAnvilHeader)) >-> (toHandle h)
     -- [(ChunkPos, ChunkData)] -> [(ChunkPos, Length)]
     -- chunks' <- mapM (\(chunkPos, (cd, modTime)) -> do i <- writeChunkData h cd ; pure (chunkPos, (i, modTime))) chunks
     pure ()
