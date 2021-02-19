{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Minecraft.Chunk where

import Control.Lens.TH (makeLenses)
import Data.Array.Unboxed     (IArray, UArray, listArray, elems)
import Data.Data (Data)
import Data.Time.Clock.POSIX (POSIXTime)
import Data.Typeable (Typeable)
import Data.NBT
import Data.Int (Int8, Int32, Int64)
import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import GHC.Generics (Generic)
import Minecraft.Core (BlockId(..), Difficulty(..), GameMode(..), ToNBT(..), ToNBTContents(..), XYZ(..), sunrise)

vectorArray :: (IArray UArray e) => Vector e -> UArray Int32 e
vectorArray v = listArray (0, fromIntegral ((Vector.length v) - 1)) (Vector.toList v)

data Section = Section
 { _Y          :: Int8 -- ^ Y index (not coordinate) of this section. Range 0 - 15.
 , _Blocks     :: Vector BlockId -- ^ 4096
 , _Add        :: Maybe (Vector Int8) -- ^ 2048
 , _BlockData  :: Vector Int8 -- ^ 2048
 , _BlockLight :: Vector Int8 -- ^ 2048
 , _SkyLight   :: Vector Int8 -- ^ 2048
 }
 deriving (Eq, Ord, Show, Typeable, Generic)
makeLenses ''Section

-- FIXME: add range checks
-- FIMXE: add 'Add' support
instance ToNBTContents Section where
  toNBTContents section =
    (CompoundTag
      [ NBT "Y" (ByteTag (_Y section))
      , NBT "Blocks" (ByteArrayTag (vectorArray (Vector.map _unBlockId (_Blocks section))))
      , NBT "Data" (ByteArrayTag (vectorArray (_BlockData section)))
--      , NBT "Add"
      , NBT "BlockLight" (ByteArrayTag (vectorArray (_BlockLight section)))
      , NBT "SkyLight" (ByteArrayTag (vectorArray (_SkyLight section)))
      ])

emptySection :: Section
emptySection = Section
  { _Y          = 0
  , _Blocks     = Vector.replicate 4096 (BlockId 0)
  , _Add        = Nothing
  , _BlockData  = Vector.replicate 2048 0
  , _BlockLight = Vector.replicate 2048 0
  , _SkyLight   = Vector.replicate 2048 0
  }

-- FIXME
data EntityData = EntityData
  {
  }
 deriving (Eq, Ord, Show, Typeable, Generic)
makeLenses ''EntityData

-- FIXME
data BlockEntity = BlockEntity
  {
  }
 deriving (Eq, Ord, Show, Typeable, Generic)
makeLenses ''BlockEntity

-- FIXME
data TileTick = TileTick
  {
  }
 deriving (Eq, Ord, Show, Typeable, Generic)
makeLenses ''TileTick

data Chunk = Chunk
  { _xPos             :: Int32
  , _zPos             :: Int32
  , _LastUpdate       :: POSIXTime
  , _LightPopulated   :: Bool
  , _TerrainPopulated :: Bool
  , _V                :: Int8
  , _InhabitedTime    :: Int64
  , _Biomes           :: Vector Int8 -- UArray Int32 Int8
  , _HeightMap        :: Vector Int32 -- UArray Int32 Int32
  , _Sections         :: [Section]
  , _Entities         :: [EntityData]
  , _TileEntities     :: [BlockEntity]
  , _TileTicks        :: [TileTick]
  }
  deriving (Eq, Ord, Show, Typeable, Generic)
makeLenses ''Chunk

emptyChunk :: Chunk
emptyChunk = Chunk
  { _xPos             = 0
  , _zPos             = 0
  , _LastUpdate       = 0
  , _LightPopulated   = False
  , _TerrainPopulated = True
  , _V                = 1
  , _InhabitedTime    = 0
  , _Biomes           = Vector.replicate 256 (-1)
  , _HeightMap        = Vector.replicate 1024 0
  , _Sections         = []
  , _Entities         = []
  , _TileEntities     = []
  , _TileTicks        = []
  }

instance ToNBTContents Chunk where
  toNBTContents chunk =
    (CompoundTag
      [ NBT "xPos" (IntTag (_xPos chunk))
      , NBT "zPos" (IntTag (_zPos chunk))
      , NBT "LastUpdate" (toNBTContents (_LastUpdate chunk))
      , NBT "LightPopulated" (toNBTContents (_LightPopulated chunk))
      , NBT "TerrainPopulated" (toNBTContents (_TerrainPopulated chunk)) --  FIXME: should be not-present for False
      , NBT "V" (ByteTag (_V chunk))
      , NBT "InhabitedTime" (LongTag (_InhabitedTime chunk))
      , NBT "Biomes" (ByteArrayTag (vectorArray (_Biomes chunk)))
      , NBT "HeightMap" (IntArrayTag (vectorArray (_HeightMap chunk)))
      , NBT "Sections" (ListTag (listArray (0, fromIntegral (length (_Sections chunk) - 1)) (map toNBTContents (_Sections chunk))))
      ])

instance ToNBT Chunk where
  toNBT chunk =
    (NBT "" (CompoundTag [ NBT "Level" (toNBTContents chunk) ]))

