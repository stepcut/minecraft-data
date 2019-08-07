{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Minecraft.Player where

import Control.Lens.TH (makeLenses)
import Data.Data (Data)
import Data.Typeable (Typeable)
import Data.NBT
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Text (Text)
import GHC.Generics (Generic)
import Minecraft.Core (Attribute(..), Dimension(..), Int6, Item(..), GameMode(..), ToNBT(..), ToNBTContents(..), XYZ(..), Pos(..), PosKind(..))
import TextShow (showt)

data Abilities = Abilities
  { _walkSpeed    :: Float -- ^ 0.1
  , _flySpeed     :: Float -- ^ 0.05
  , _mayFly       :: Bool -- ^ true
  , _flying       :: Bool -- ^ false
  , _invulnerable :: Bool -- ^ false
  , _mayBuild     :: Bool -- ^ true
  , _instabuild   :: Bool -- ^ false
  }
  deriving (Eq, Ord, Read, Show, Data, Typeable, Generic)
makeLenses ''Abilities

instance ToNBT Abilities where
  toNBT abilities =
    NBT "abilities" (toNBTContents abilities)

instance ToNBTContents Abilities where
  toNBTContents abilities =
    (CompoundTag
      [ NBT "walkSpeed"    (FloatTag (_walkSpeed abilities))
      , NBT "flySpeed"     (FloatTag (_flySpeed abilities))
      , NBT "mayfly"       (toNBTContents (_mayFly abilities))
      , NBT "flying"       (toNBTContents (_flying abilities))
      , NBT "invulnerable" (toNBTContents (_invulnerable abilities))
      , NBT "mayBuild"     (toNBTContents (_mayBuild abilities))
      , NBT "instabuild"   (toNBTContents (_instabuild abilities))
      ])

defaultAbilities :: Abilities
defaultAbilities = Abilities
  { _walkSpeed    = 0.1
  , _flySpeed     = 0.05
  , _mayFly       = True
  , _flying       = False
  , _invulnerable = False
  , _mayBuild     = True
  , _instabuild   = False
  }

data InventoryItem = InventoryItem
  { _slot :: Int6
  , _item :: Item
  , _count :: Int6
  , _damage :: Int16
  }
  deriving (Eq, Ord, Read, Show, Data, Typeable, Generic)
makeLenses ''InventoryItem

instance ToNBTContents InventoryItem where
  toNBTContents ii =
    CompoundTag [ NBT "Slot"   (ByteTag (_slot ii))
                , NBT "id"     (StringTag (showt (_item ii)))
                , NBT "Count"  (ByteTag (_count ii))
                , NBT "Damage" (ShortTag (_damage ii))
                ]

data EnderItem = EnderItem
  deriving (Eq, Ord, Read, Show, Data, Typeable, Generic)

instance ToNBTContents EnderItem where
  toNBTContents EnderItem = error "ToNBTContents EnterItem not implemented."

data Player = Player
  { _dimension           :: Dimension
  , _playerGameType      :: GameMode
  , _score               :: Int32
  , _selectedItemSlot    :: Int32
  , _playerSpawn         :: Maybe XYZ
--   , _spawnForced :: Bool
  , _sleeping            :: Bool
  , _sleepTimer          :: Int16 -- ^ 0, no effect
  , _fire                :: Int16 -- ^ -20
  , _foodLevel           :: Int32 -- ^ 20 is full
  , _foodExhaustionLevel :: Float -- ^ 0
  , _foodSaturationLevel :: Float -- ^ 5
  , _foodTickTimer       :: Int32 -- ^ 0
  , _xpLevel             :: Int32 -- ^ 0
  , _xpP                 :: Float -- ^ 0
  , _xpTotal             :: Int32 -- ^ 0
  , _xpSeed              :: Int32
  , _inventory           :: [InventoryItem]
  , _enderItems          :: [EnderItem]
  , _abilities           :: Abilities
  , _hurtByTimestamp     :: Int32 -- ^ 0
  , _hurtTime            :: Int16 -- ^ 0
  , _attributes          :: [Attribute]
  }
  deriving (Eq, Ord, Read, Show, Data, Typeable, Generic)
makeLenses ''Player

instance ToNBT Player where
  toNBT player =
    NBT "Player" (toNBTContents player)

-- FIXME: what happens if the player spawn position includes tilda/caret
instance ToNBTContents Player where
  toNBTContents player = (CompoundTag
      ((case _playerSpawn player of
          Nothing -> []
          (Just (XYZ (Pos Abs x) (Pos Abs y) (Pos Abs z))) ->
            [ NBT "SpawnX" (toNBTContents x)
            , NBT "SpawnY" (toNBTContents y)
            , NBT "SpawnZ" (toNBTContents z)
            ]) ++
      [ NBT "Dimension" (toNBTContents (_dimension player))
      , NBT "GameType"  (toNBTContents (_playerGameType player))
      , NBT "Score"     (toNBTContents (_score player))
      , NBT "SelectedItemSlot" (toNBTContents (_selectedItemSlot player))
      , NBT "Sleeping"  (toNBTContents (_sleeping player))
      , NBT "SleepTimer" (toNBTContents (_sleepTimer player))
      , NBT "Fire"       (toNBTContents (_fire player))
      , NBT "foodLevel" (toNBTContents (_foodLevel player))
      , NBT "foodExhaustionLevel" (toNBTContents (_foodExhaustionLevel player))
      , NBT "foodSaturationLevel" (toNBTContents (_foodSaturationLevel player))
      , NBT "foodTickTimer"       (toNBTContents (_foodTickTimer player))
      , NBT "XpLevel"             (toNBTContents (_xpLevel player))
      , NBT "XpP"                 (toNBTContents (_xpP player))
      , NBT "XpTotal"             (toNBTContents (_xpTotal player))
      , NBT "Inventory"           (toNBTContents (_inventory player))
      , NBT "EnderItems"          (toNBTContents (_enderItems player))
      , toNBT (_abilities player)
      , NBT "HurtByTimestamp"     (toNBTContents (_hurtByTimestamp player))
      , NBT "HurtTime"            (toNBTContents (_hurtTime player))
      , NBT "Attributes"          (toNBTContents (_attributes player))
      ]))

defaultPlayer :: Int32 -- ^ XpSeed
              -> Player
defaultPlayer seed = Player
  { _dimension           = Overworld
  , _playerGameType      = SurvivalMode
  , _score               = 0
  , _selectedItemSlot    = 0
  , _playerSpawn         = Nothing
  , _sleeping            = False
  , _sleepTimer          = 0
  , _fire                = (-20)
  , _foodLevel           = 20
  , _foodExhaustionLevel = 0
  , _foodSaturationLevel = 5
  , _foodTickTimer       = 0
  , _xpLevel             = 0
  , _xpP                 = 0
  , _xpTotal             = 0
  , _xpSeed              = seed
  , _inventory           = []
  , _enderItems          = []
  , _abilities           = defaultAbilities
  , _hurtByTimestamp     = 0
  , _hurtTime            = 0
  , _attributes          = []
  }
