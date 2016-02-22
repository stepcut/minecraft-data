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



{-
NBT "" (CompoundTag [NBT "Data" (CompoundTag [NBT "RandomSeed" (LongTag (-109338057461131572)),NBT "generatorName" (StringTag "flat"),NBT "BorderCenterZ" (DoubleTag 0.0),NBT "Difficulty" (ByteTag 2),NBT "BorderSizeLerpTime" (LongTag 0),NBT "raining" (ByteTag 0),NBT "Time" (LongTag 28277),NBT "GameType" (IntTag 1),NBT "MapFeatures" (ByteTag 1),NBT "BorderCenterX" (DoubleTag 0.0),NBT "BorderDamagePerBlock" (DoubleTag 0.2),NBT "BorderWarningBlocks" (DoubleTag 5.0),NBT "BorderSizeLerpTarget" (DoubleTag 6.0e7),NBT "DayTime" (LongTag 226),NBT "initialized" (ByteTag 1),NBT "allowCommands" (ByteTag 1),NBT "SizeOnDisk" (LongTag 0),NBT "GameRules" (CompoundTag [NBT "doTileDrops" (StringTag "false"),NBT "doFireTick" (StringTag "true"),NBT "reducedDebugInfo" (StringTag "false"),NBT "naturalRegeneration" (StringTag "true"),NBT "doMobLoot" (StringTag "true"),NBT "keepInventory" (StringTag "false"),NBT "mobGriefing" (StringTag "true"),NBT "randomTickSpeed" (StringTag "3"),NBT "commandBlockOutput" (StringTag "false"),NBT "doMobSpawning" (StringTag "false"),NBT "logAdminCommands" (StringTag "true"),NBT "sendCommandFeedback" (StringTag "true"),NBT "doDaylightCycle" (StringTag "false"),NBT "showDeathMessages" (StringTag "true")]),NBT "Player" (CompoundTag [NBT "HurtByTimestamp" (IntTag 0),NBT "SleepTimer" (ShortTag 0),NBT "Attributes" (ListTag (array (0,3) [(0,CompoundTag [NBT "Base" (DoubleTag 20.0),NBT "Name" (StringTag "generic.maxHealth")]),(1,CompoundTag [NBT "Base" (DoubleTag 0.0),NBT "Name" (StringTag "generic.knockbackResistance")]),(2,CompoundTag [NBT "Base" (DoubleTag 0.10000000149011612),NBT "Name" (StringTag "generic.movementSpeed")]),(3,CompoundTag [NBT "Base" (DoubleTag 1.0),NBT "Name" (StringTag "generic.attackDamage")])])),NBT "Invulnerable" (ByteTag 0),NBT "PortalCooldown" (IntTag 0),NBT "AbsorptionAmount" (FloatTag 0.0),NBT "abilities" (CompoundTag [NBT "invulnerable" (ByteTag 1),NBT "mayfly" (ByteTag 1),NBT "instabuild" (ByteTag 1),NBT "walkSpeed" (FloatTag 0.1),NBT "mayBuild" (ByteTag 1),NBT "flying" (ByteTag 1),NBT "flySpeed" (FloatTag 5.0e-2)]),NBT "FallDistance" (FloatTag 14.416479),NBT "DeathTime" (ShortTag 0),NBT "XpSeed" (IntTag (-1755376651)),NBT "HealF" (FloatTag 20.0),NBT "XpTotal" (IntTag 0),NBT "playerGameType" (IntTag 1),NBT "Motion" (ListTag (array (0,2) [(0,DoubleTag 0.0),(1,DoubleTag 0.0),(2,DoubleTag 0.0)])),NBT "UUIDLeast" (LongTag (-7674417195992141339)),NBT "Health" (ShortTag 20),NBT "foodSaturationLevel" (FloatTag 5.0),NBT "Air" (ShortTag 300),NBT "OnGround" (ByteTag 0),NBT "Dimension" (IntTag 0),NBT "Rotation" (ListTag (array (0,1) [(0,FloatTag (-340.48886)),(1,FloatTag 50.39998)])),NBT "XpLevel" (IntTag 0),NBT "Score" (IntTag 0),NBT "UUIDMost" (LongTag 8478971869002416450),NBT "Sleeping" (ByteTag 0),NBT "Pos" (ListTag (array (0,2) [(0,DoubleTag (-209.8169138127368)),(1,DoubleTag 100.45194236716503),(2,DoubleTag (-81.85082891485048))])),NBT "Fire" (ShortTag (-20)),NBT "XpP" (FloatTag 0.0),NBT "EnderItems" (ListTag (array (0,-1) [])),NBT "foodLevel" (IntTag 20),NBT "foodExhaustionLevel" (FloatTag 0.0),NBT "HurtTime" (ShortTag 0),NBT "SelectedItemSlot" (IntTag 1),NBT "Inventory" (ListTag (array (0,30) [(0,CompoundTag [NBT "Slot" (ByteTag 0),NBT "id" (StringTag "minecraft:bow"),NBT "Count" (ByteTag 1),NBT "tag" (CompoundTag [NBT "ench" (ListTag (array (0,0) [(0,CompoundTag [NBT "lvl" (ShortTag 1),NBT "id" (ShortTag 50)])]))]),NBT "Damage" (ShortTag 0)]),(1,CompoundTag [NBT "Slot" (ByteTag 9),NBT "id" (StringTag "minecraft:pumpkin"),NBT "Count" (ByteTag 1),NBT "Damage" (ShortTag 0)]),(2,CompoundTag [NBT "Slot" (ByteTag 10),NBT "id" (StringTag "minecraft:wooden_button"),NBT "Count" (ByteTag 1),NBT "Damage" (ShortTag 0)]),(3,CompoundTag [NBT "Slot" (ByteTag 11),NBT "id" (StringTag "minecraft:log"),NBT "Count" (ByteTag 1),NBT "Damage" (ShortTag 1)]),(4,CompoundTag [NBT "Slot" (ByteTag 12),NBT "id" (StringTag "minecraft:dark_oak_door"),NBT "Count" (ByteTag 2),NBT "Damage" (ShortTag 0)]),(5,CompoundTag [NBT "Slot" (ByteTag 13),NBT "id" (StringTag "minecraft:redstone"),NBT "Count" (ByteTag 2),NBT "Damage" (ShortTag 0)]),(6,CompoundTag [NBT "Slot" (ByteTag 14),NBT "id" (StringTag "minecraft:golden_rail"),NBT "Count" (ByteTag 1),NBT "Damage" (ShortTag 0)]),(7,CompoundTag [NBT "Slot" (ByteTag 15),NBT "id" (StringTag "minecraft:chest_minecart"),NBT "Count" (ByteTag 1),NBT "Damage" (ShortTag 0)]),(8,CompoundTag [NBT "Slot" (ByteTag 16),NBT "id" (StringTag "minecraft:coal"),NBT "Count" (ByteTag 1),NBT "Damage" (ShortTag 0)]),(9,CompoundTag [NBT "Slot" (ByteTag 17),NBT "id" (StringTag "minecraft:hopper_minecart"),NBT "Count" (ByteTag 1),NBT "Damage" (ShortTag 0)]),(10,CompoundTag [NBT "Slot" (ByteTag 18),NBT "id" (StringTag "minecraft:tnt_minecart"),NBT "Count" (ByteTag 1),NBT "Damage" (ShortTag 0)]),(11,CompoundTag [NBT "Slot" (ByteTag 20),NBT "id" (StringTag "minecraft:furnace_minecart"),NBT "Count" (ByteTag 1),NBT "Damage" (ShortTag 0)]),(12,CompoundTag [NBT "Slot" (ByteTag 21),NBT "id" (StringTag "minecraft:obsidian"),NBT "Count" (ByteTag 1),NBT "Damage" (ShortTag 0)]),(13,CompoundTag [NBT "Slot" (ByteTag 22),NBT "id" (StringTag "minecraft:lever"),NBT "Count" (ByteTag 2),NBT "Damage" (ShortTag 0)]),(14,CompoundTag [NBT "Slot" (ByteTag 23),NBT "id" (StringTag "minecraft:command_block"),NBT "Count" (ByteTag 1),NBT "Damage" (ShortTag 0)]),(15,CompoundTag [NBT "Slot" (ByteTag 24),NBT "id" (StringTag "minecraft:slime"),NBT "Count" (ByteTag 1),NBT "Damage" (ShortTag 0)]),(16,CompoundTag [NBT "Slot" (ByteTag 25),NBT "id" (StringTag "minecraft:comparator"),NBT "Count" (ByteTag 1),NBT "Damage" (ShortTag 0)]),(17,CompoundTag [NBT "Slot" (ByteTag 26),NBT "id" (StringTag "minecraft:repeater"),NBT "Count" (ByteTag 2),NBT "Damage" (ShortTag 0)]),(18,CompoundTag [NBT "Slot" (ByteTag 27),NBT "id" (StringTag "minecraft:sticky_piston"),NBT "Count" (ByteTag 1),NBT "Damage" (ShortTag 0)]),(19,CompoundTag [NBT "Slot" (ByteTag 28),NBT "id" (StringTag "minecraft:flint_and_steel"),NBT "Count" (ByteTag 1),NBT "Damage" (ShortTag 0)]),(20,CompoundTag [NBT "Slot" (ByteTag 29),NBT "id" (StringTag "minecraft:slime"),NBT "Count" (ByteTag 1),NBT "Damage" (ShortTag 0)]),(21,CompoundTag [NBT "Slot" (ByteTag 30),NBT "id" (StringTag "minecraft:redstone_torch"),NBT "Count" (ByteTag 1),NBT "Damage" (ShortTag 0)]),(22,CompoundTag [NBT "Slot" (ByteTag 31),NBT "id" (StringTag "minecraft:stone"),NBT "Count" (ByteTag 2),NBT "Damage" (ShortTag 0)]),(23,CompoundTag [NBT "Slot" (ByteTag 32),NBT "id" (StringTag "minecraft:armor_stand"),NBT "Count" (ByteTag 1),NBT "Damage" (ShortTag 0)]),(24,CompoundTag [NBT "Slot" (ByteTag 33),NBT "id" (StringTag "minecraft:coal"),NBT "Count" (ByteTag 1),NBT "Damage" (ShortTag 0)]),(25,CompoundTag [NBT "Slot" (ByteTag 34),NBT "id" (StringTag "minecraft:stone_pressure_plate"),NBT "Count" (ByteTag 1),NBT "Damage" (ShortTag 0)]),(26,CompoundTag [NBT "Slot" (ByteTag 35),NBT "id" (StringTag "minecraft:tnt"),NBT "Count" (ByteTag 1),NBT "Damage" (ShortTag 0)]),(27,CompoundTag [NBT "Slot" (ByteTag 100),NBT "id" (StringTag "minecraft:leather_boots"),NBT "Count" (ByteTag 1),NBT "Damage" (ShortTag 0)]),(28,CompoundTag [NBT "Slot" (ByteTag 101),NBT "id" (StringTag "minecraft:leather_leggings"),NBT "Count" (ByteTag 1),NBT "Damage" (ShortTag 0)]),(29,CompoundTag [NBT "Slot" (ByteTag 102),NBT "id" (StringTag "minecraft:leather_chestplate"),NBT "Count" (ByteTag 1),NBT "Damage" (ShortTag 0)]),(30,CompoundTag [NBT "Slot" (ByteTag 103),NBT "id" (StringTag "minecraft:leather_helmet"),NBT "Count" (ByteTag 1),NBT "Damage" (ShortTag 0)])])),NBT "foodTickTimer" (IntTag 0)]),NBT "SpawnY" (IntTag 4),NBT "rainTime" (IntTag 126297),NBT "thunderTime" (IntTag 15881),NBT "SpawnZ" (IntTag 342),NBT "hardcore" (ByteTag 0),NBT "DifficultyLocked" (ByteTag 0),NBT "SpawnX" (IntTag (-365)),NBT "clearWeatherTime" (IntTag 0),NBT "thundering" (ByteTag 0),NBT "generatorVersion" (IntTag 0),NBT "version" (IntTag 19133),NBT "BorderSafeZone" (DoubleTag 5.0),NBT "generatorOptions" (StringTag "3;minecraft:bedrock,3*minecraft:stone,52*minecraft:sandstone;2;"),NBT "LastPlayed" (LongTag 1453648356720),NBT "BorderWarningTime" (DoubleTag 15.0),NBT "LevelName" (StringTag "Danger casal"),NBT "BorderSize" (DoubleTag 6.0e7)])])
-}
