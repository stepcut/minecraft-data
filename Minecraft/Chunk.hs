{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Minecraft.Chunk where

import Control.Lens.TH (makeLenses)
import Data.Array.Unboxed     (UArray, listArray, elems)
import Data.Data (Data)
import Data.Time.Clock.POSIX (POSIXTime)
import Data.Typeable (Typeable)
import Data.NBT
import Data.Int (Int8, Int32, Int64)
import Data.Text (Text)
import GHC.Generics (Generic)
import Minecraft.Core (Difficulty(..), GameMode(..), ToNBT(..), ToNBTContents(..), XYZ(..), sunrise)

data Chunk = Chunk
  { _xPos             :: Int32
  , _zPos             :: Int32
  , _LastUpdate       :: POSIXTime
  , _LightPopulated   :: Bool
  , _TerrainPopulated :: Bool
  , _V                :: Int8
  , _InhabitedTime    :: Int64
  , _Biomes           :: UArray Int32 Int8
  , _HeightMap        :: UArray Int32 Int32
--  , _Sections         :: [Section]
--  , _Entities         :: [Entity]
--  , _TileEntities     :: [TileEntity]
--  , _TileTicks        :: [TileTick]
  }
  deriving (Eq, Ord, Show, Typeable, Generic)
makeLenses ''Chunk
