{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Minecraft.Level where

import Control.Lens.TH (makeLenses)
import Data.Data (Data)
import Data.Time.Clock.POSIX (POSIXTime)
import Data.Typeable (Typeable)
import Data.NBT
import Data.Int (Int32, Int64)
import Data.Text (Text)
import GHC.Generics (Generic)
import Minecraft.Core (Difficulty(..), GameMode(..), ToNBT(..), ToNBTContents(..), XYZ(..), sunrise)
import Minecraft.Player (Player(..), defaultPlayer)

data GeneratorName
  = Default
  | Flat
  | LargeBiomes
  | Amplified
  | Customized
  | DebugAllBlockStates
  deriving (Eq, Ord, Read, Show, Data, Typeable, Generic)

instance ToNBTContents GeneratorName where
  toNBTContents generatorName =
    StringTag $ case generatorName of
      Default             -> "default"
      Flat                -> "flat"
      LargeBiomes         -> "largeBiomes"
      Amplified           -> "amplified"
      Customized          -> "customized"
      DebugAllBlockStates -> "debug_all_block_states"

data GameRules = GameRules
  { _commandBlockOutput  :: Bool -- ^ True
  , _doDaylightCycle     :: Bool -- ^ True
  , _doFireTick          :: Bool -- ^ True
  , _doMobLoot           :: Bool -- ^ True
  , _doMobSpawning       :: Bool -- ^ True
  , _doTileDrops         :: Bool -- ^ True
  , _keepInventory       :: Bool -- ^ False
  , _logAdminCommands    :: Bool -- ^ True
  , _mobGriefing         :: Bool -- ^ True
  , _naturalRegeneration :: Bool -- ^ True
  , _randomTickSpeed     :: Int32 -- ^ 3
  , _sendCommandFeedback :: Bool -- ^ True
  , _showDeathMessages   :: Bool -- ^ True
  , _reducedDebugInfo    :: Bool -- ^ False
  }
  deriving (Eq, Ord, Read, Show, Data, Typeable, Generic)

defaultGameRules :: GameRules
defaultGameRules = GameRules
  { _commandBlockOutput  = True
  , _doDaylightCycle     = True
  , _doFireTick          = True
  , _doMobLoot           = True
  , _doMobSpawning       = True
  , _doTileDrops         = True
  , _keepInventory       = False
  , _logAdminCommands    = True
  , _mobGriefing         = True
  , _naturalRegeneration = True
  , _randomTickSpeed     = 3
  , _sendCommandFeedback = True
  , _showDeathMessages   = True
  , _reducedDebugInfo    = False
  }

instance ToNBTContents GameRules where
  toNBTContents gameRules =
    CompoundTag
      [ NBT "commandBlockOutput"  (boolString $ _commandBlockOutput gameRules)
      , NBT "doDaylightCycle"     (boolString $ _doDaylightCycle gameRules)
      , NBT "doFireTick"          (boolString $ _doFireTick gameRules)
      , NBT "doMobLoot"           (boolString $ _doMobLoot gameRules)
      , NBT "doMobSpawning"       (boolString $ _doMobSpawning gameRules)
      , NBT "doTileDrops"         (boolString $ _doTileDrops gameRules)
      , NBT "keepInventory"       (boolString $ _keepInventory gameRules)
      , NBT "logAdminCommands"    (boolString $ _logAdminCommands gameRules)
      , NBT "mobGriefing"         (boolString $ _mobGriefing gameRules)
      , NBT "naturalRegeneration" (boolString $ _naturalRegeneration gameRules)
      , NBT "randomTickSpeed"     (toNBTContents $ _randomTickSpeed gameRules)
      , NBT "sendCommandFeedback" (boolString $ _sendCommandFeedback gameRules)
      , NBT "showDeathMessages"   (boolString $ _showDeathMessages gameRules)
      , NBT "reducedDebugInfo"    (boolString $ _reducedDebugInfo gameRules)
      ]
    where
      boolString False = StringTag "false"
      boolString True  = StringTag "true"

instance ToNBT GameRules where
  toNBT gameRules = NBT "GameRules" (toNBTContents gameRules)

data Version = Version
  { _versionId       :: Text
  , _versionName     :: Text
  , _versionSnapshot :: Bool
  }

data Level = Level
  { _version              :: Int32
  , _initialized          :: Bool
  , _levelName            :: Text
  , _generatorName        :: GeneratorName
  , _generatorVersion     :: Int32 -- ^ usually 0
  , _generatorOptions     :: Text
  , _randomSeed           :: Int64
  , _mapFeatures          :: Bool
  , _lastPlayed           :: POSIXTime
  , _sizeOnDisk           :: Int64 -- ^ currently unused
  , _allowCommands        :: Bool
  , _hardcore             :: Bool
  , _gameType             :: GameMode
  , _difficulty           :: Difficulty -- ^ default Normal
  , _difficultyLocked     :: Bool
  , _time                 :: Int64
  , _dayTime              :: Int64
  , _spawn                :: XYZ
  , _borderCenterX        :: Double
  , _borderCenterZ        :: Double
  , _borderSize           :: Double -- ^ default 6.0e7
  , _borderSafeZone       :: Double -- ^ default 5
  , _borderWarningBlocks  :: Double -- ^ default 5
  , _borderWarningTime    :: Double -- ^ default 15
  , _borderSizeLerpTarget :: Double -- ^ 6.0e7
  , _borderSizeLerpTime   :: Int64 -- ^ default 0
  , _borderDamagePerBlock :: Double -- ^ default 0.2
  , _raining              :: Bool
  , _rainTime             :: Int32
  , _thundering           :: Bool
  , _thunderTime          :: Int32
  , _clearWeatherTime     :: Int32
  , _player               :: Player
  }

instance ToNBTContents Level where
  toNBTContents level = CompoundTag
    [ NBT "version" (toNBTContents (_version level))
    , NBT "initialized" (toNBTContents (_initialized level))
    , NBT "generatorName" (toNBTContents (_generatorName level))
    , NBT "generatorVersion" (toNBTContents (_generatorVersion level))
    , NBT "generatorOptions" (toNBTContents (_generatorOptions level))
    , NBT "RandomSeed" (toNBTContents (_randomSeed level))
    , NBT "MapFeatures" (toNBTContents (_mapFeatures level))
    , NBT "LastPlayed" (toNBTContents (_lastPlayed level))
    , NBT "SizeOnDisk" (toNBTContents (_sizeOnDisk level))
    , NBT "allowCommands" (toNBTContents (_allowCommands level))
    , NBT "hardcore" (toNBTContents (_hardcore level))
    , NBT "GameType" (toNBTContents (_gameType level))
    , NBT "Difficulty" (toNBTContents (_difficulty level))
    , NBT "DifficultyLocked" (toNBTContents (_difficultyLocked level))
    , NBT "Time" (toNBTContents (_time level))
    , NBT "DayTime" (toNBTContents (_dayTime level))
    , NBT "SpawnX" (toNBTContents (_x (_spawn level)))
    , NBT "SpawnY" (toNBTContents (_y (_spawn level)))
    , NBT "SpawnZ" (toNBTContents (_z (_spawn level)))
    , NBT "BorderCenterX" (toNBTContents (_borderCenterX level))
    , NBT "BorderCenterZ" (toNBTContents (_borderCenterZ level))
    , NBT "BorderSize" (toNBTContents (_borderSize level))
    , NBT "BorderSafeZone" (toNBTContents (_borderSafeZone level))
    , NBT "BorderWarningBlocks" (toNBTContents (_borderWarningBlocks level))
    , NBT "BorderSizeLerpTarget" (toNBTContents (_borderSizeLerpTarget level))
    , NBT "BorderSizeLerpTime" (toNBTContents (_borderSizeLerpTime level))
    , NBT "BorderDamagePerBlock" (toNBTContents (_borderDamagePerBlock level))
    , NBT "raining" (toNBTContents (_raining level))
    , NBT "rainTime" (toNBTContents (_rainTime level))
    , NBT "thundering" (toNBTContents (_thundering level))
    , NBT "thunderTime" (toNBTContents (_thunderTime level))
    , NBT "clearWeatherTime" (toNBTContents (_clearWeatherTime level))
    , NBT "Player" (toNBTContents (_player level))
    ]

instance ToNBT Level where
  toNBT level =
    NBT "" (CompoundTag [NBT "Data" (toNBTContents level)])
defaultLevel :: Text -- ^ level name
             -> Int64 -- ^ random seed
             -> Int32 -- ^ XpSeed
             -> Level
defaultLevel lvlName rSeed xpSeed = Level
  { _version              = 19133
  , _initialized          = True
  , _levelName            = lvlName
  , _generatorName        = Default
  , _generatorVersion     = 0
  , _generatorOptions     = ""
  , _randomSeed           = rSeed
  , _mapFeatures          = True
  , _lastPlayed           = 0
  , _sizeOnDisk           = 0
  , _allowCommands        = False
  , _hardcore             = False
  , _gameType             = SurvivalMode
  , _difficulty           = Normal
  , _difficultyLocked     = True
  , _time                 = 0
  , _dayTime              = sunrise
  , _spawn                = XYZ 0 0 0
  , _borderCenterX        = 0.0
  , _borderCenterZ        = 0.0
  , _borderSize           = 6.0e7
  , _borderSafeZone       = 5.0
  , _borderWarningBlocks  = 5.0
  , _borderWarningTime    = 15.0
  , _borderSizeLerpTarget = 6.0e7
  , _borderSizeLerpTime   = 0
  , _borderDamagePerBlock = 0.2
  , _raining              = False
  , _rainTime             = 0
  , _thundering           = False
  , _thunderTime          = 0
  , _clearWeatherTime     = 0
  , _player               = defaultPlayer xpSeed
  }
