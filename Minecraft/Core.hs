{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Minecraft.Core where

import Control.Lens.TH (makeLenses)
import qualified Data.Array as Array
import Data.Data
import Data.List (intersperse)
import Data.Maybe (maybe, fromMaybe)
import Data.Monoid ((<>), mconcat, mempty)
import Data.NBT
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Time.Clock.POSIX (POSIXTime)
import Data.Word (Word, Word8)
import Data.Text (Text)
import Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as B
import GHC.Generics
import TextShow

type Int6 = Int8

class Render a where
  render :: a -> Builder

instance Render a => Render (Maybe a) where
  render Nothing = mempty
  render (Just a) = render a

instance Render Int6 where
  render i = B.fromString (show i)

instance Render Int64 where
  render i = B.fromString (show i)

class ToNBT a where
  toNBT :: a -> NBT

class ToNBTContents a where
  toNBTContents :: a -> NbtContents

instance ToNBTContents Bool where
  toNBTContents False = ByteTag 0
  toNBTContents True  = ByteTag 1

instance ToNBTContents Int8 where
  toNBTContents i = ByteTag i

instance ToNBTContents Int16 where
  toNBTContents i = ShortTag i

instance ToNBTContents Int32 where
  toNBTContents i = IntTag i

instance ToNBTContents Int64 where
  toNBTContents i = LongTag i

instance ToNBTContents Float where
  toNBTContents f = FloatTag f

instance ToNBTContents Double where
  toNBTContents d = DoubleTag d

instance ToNBTContents Text where
  toNBTContents t = StringTag t

instance ToNBTContents POSIXTime where
  toNBTContents t = LongTag (round (t * 1000))

instance ToNBTContents a => ToNBTContents [a] where
  toNBTContents l = ListTag (Array.listArray (0, (fromIntegral $ length l) - 1) (map toNBTContents l))

instance ToNBTContents [NbtContents] where
  toNBTContents l = ListTag (Array.listArray (0, (fromIntegral $ length l) - 1) l)

data Coordinate
  = Absolute Int
  | Relative Int
    deriving (Eq, Ord, Read, Show, Data, Typeable, Generic)

instance Render Coordinate where
  render (Absolute i) = B.fromString (show i)
  render (Relative 0) = B.singleton '~'
  render (Relative i) = B.singleton '~' <> B.fromString (show i)

data TargetVariable
  = NearestPlayer
  | RandomPlayer
  | AllPlayers
  | AllEntities
    deriving (Eq, Ord, Read, Show, Data, Typeable, Generic)

instance Render TargetVariable where
  render t =
    case t of
     NearestPlayer -> "@p"
     RandomPlayer  -> "@r"
     AllPlayers    -> "@a"
     AllEntities   -> "@e"

type Name = Text

instance Render Text where
  render t = B.fromText t

data GameMode
  = AllModes
  | SurvivalMode
  | CreativeMode
  | AdventureMode
  | SpectatorMode
    deriving (Eq, Ord, Read, Show, Data, Typeable, Generic)

instance Render GameMode where
  render m =
    case m of
     AllModes      -> "-1"
     SurvivalMode  -> "0"
     CreativeMode  -> "1"
     AdventureMode -> "2"
     SpectatorMode -> "3"

instance ToNBTContents GameMode where
  toNBTContents gm =
    IntTag $ case gm of
      AllModes      -> (-1)
      SurvivalMode  -> 0
      CreativeMode  -> 1
      AdventureMode -> 2
      SpectatorMode -> 3


newtype BlockId = BlockId { _unBlockId:: Int8 }
    deriving (Eq, Ord, Read, Show, Data, Typeable, Generic)
makeLenses ''BlockId

newtype EntityId = EntityId { unEntityId:: Int8 }
    deriving (Eq, Ord, Read, Show, Data, Typeable, Generic)

data Entity
  -- Drops
  = Item
  | XPOrb
  -- Immobile
  | LeashKnot
  | Painting
  | ItemFrame
  | ArmorStand
  | EnderCrystal
  -- Projectiles
  | ThrownEgg
  | Arrow
  | Snowball
  | Fireball
  | SmallFireball
  | ThrownEnderpearl
  | EyeOfEnderSignal
  | ThrownPotion
  | ThrownExpBottle
  | WitherSkull
  | FireworksRocketEntity
  -- Blocks
  | PrimedTnt
  | FailingSand
  -- Vehicles
  | MinecartCommandBlock
  | Boat
  | MinecartRideable
  | MinecartChest
  | MinecartFurnace
  | MinecartTNT
  | MinecartHopper
  | MinecartSpawner
  -- Generic
  | Mob
  | Monster
  -- Hostile Mobs
  | Creeper
  | Skeleton
  | Spider
  | Giant
  | Zombie
  | Slime
  | Ghast
  | PigZombie
  | Enderman
  | CaveSpider
  | Silverfish
  | Blaze
  | LavaSlime
  | EnderDragon
  | WitherBoss
  | Witch
  | Endermite
  | Guardian
  | Shulker
--  | KillerRabbit -- ?
  -- Passive Mobs
  | Bat
  | Pig
  | Sheep
  | Cow
  | Chicken
  | Squid
  | Wolf
  | MooshroomCow
  | SnowMan
  | Ozelot
  | VillagerGolem
  | EntityHorse
  | Rabbit
  -- NPCs
  | Villager
    deriving (Eq, Ord, Read, Show, Data, Typeable, Generic)

toEntityId :: Entity -> EntityId
toEntityId entity =
  case entity of
   Item -> EntityId 1
   Creeper     -> EntityId 50

instance Render EntityId where
  render (EntityId n) = B.fromString (show n)

instance Render Entity where
  render e =
    case e of
     Item        -> "Item"
     XPOrb       -> "XPOrb"
     LeashKnot   -> "LeashKnot"
     Painting    -> "Painting"
     ItemFrame   -> "ItemFrame"
     ArmorStand -> "ArmorStand"
     EnderCrystal -> "EnderCrystal"
     -- Projectiles
     ThrownEgg -> "ThrownEgg"
     Arrow -> "Arrow"
     Snowball -> "Snowball"
     Fireball -> "Fireball"
     SmallFireball -> "SmallFireball"
     ThrownEnderpearl -> "ThrownEnderpearl"
     EyeOfEnderSignal -> "EyeOfEnderSignal"
     ThrownPotion -> "ThrownPotion"
     ThrownExpBottle -> "ThrownExpBottle"
     WitherSkull -> "WitherSkull"
     FireworksRocketEntity -> "FireworksRocketEntity"
     -- Blocks
     PrimedTnt -> "PrimedTnt"
     FailingSand -> "FailingSand"
     -- Vehicles
     MinecartCommandBlock -> "MinecartCommandBlock"
     Boat -> "Boat"
     MinecartRideable -> "MinecartRideable"
     MinecartChest -> "MinecartChest"
     MinecartFurnace -> "MinecartFurnace"
     MinecartTNT -> "MinecartTNT"
     MinecartHopper -> "MinecartHopper"
     MinecartSpawner -> "MinecartSpawner"
     -- Generic
     Mob -> "Mob"
     Monster -> "Monster"
     -- Hostile Mobs
     Creeper -> "Creeper"
     Skeleton -> "Skeleton"
     Spider -> "Spider"
     Giant -> "Giant"
     Zombie -> "Zombie"
     Slime -> "Slime"
     Ghast -> "Ghast"
     PigZombie -> "PigZombie"
     Enderman -> "Enderman"
     CaveSpider -> "CaveSpider"
     Silverfish -> "Silverfish"
     Blaze -> "Blaze"
     LavaSlime -> "LavaSlime"
     EnderDragon -> "EnderDragon"
     WitherBoss -> "WitherBoss"
     Witch -> "Witch"
     Endermite -> "Endermite"
     Guardian -> "Guardian"
     Shulker -> "Shulker"
--     Rabbit -> "Rabbit"
     -- Passive Mobs
     Bat -> "Bat"
     Pig -> "Pig"
     Sheep -> "Sheep"
     Cow -> "Cow"
     Chicken -> "Chicken"
     Squid -> "Squid"
     Wolf -> "Wolf"
     MooshroomCow -> "MooshroomCow"
     SnowMan -> "SnowMan"
     Ozelot -> "Ozelot"
     VillagerGolem -> "VillagerGolem"
     EntityHorse -> "EntityHorse"
     Rabbit -> "Rabbit"
     -- NPCs
     Villager -> "Villager"

data Op
  = Equal
  | NotEqual
    deriving (Eq, Ord, Read, Show, Data, Typeable, Generic)

instance Render Op where
  render op =
    case op of
     Equal      -> "="
     NotEqual  -> "=!"

data TargetArgument
  = TargetX Coordinate
  | TargetY Coordinate
  | TargetZ Coordinate
  | RadiusMax Coordinate
  | RadiusMin Coordinate
  | GameMode GameMode
  | Count Int
  | XpLevelMax Word
  | XpLevelMin Word
  | ScoreMax Name Word
  | ScoreMin Name Word
  | Team Op Name
  | Name Op Name
  | Dx Coordinate
  | Dy Coordinate
  | Dz Coordinate
  | RxMax Double
  | RxMin Double
  | RyMax Double
  | RyMin Double
  | Type Op Entity
    deriving (Eq, Ord, Read, Show, Data, Typeable, Generic)

instance Render TargetArgument where
  render arg =
    case arg of
     (Count n) -> "count="<>B.fromString (show n)
     (Type op entity) -> "type" <> render op <> render entity

data TargetSelector = TargetSelector TargetVariable [TargetArgument]
    deriving (Eq, Ord, Read, Show, Data, Typeable, Generic)

instance Render TargetSelector where
  render (TargetSelector var []) =
    render var
  render (TargetSelector var args) =
    render var <> B.singleton '[' <> mconcat (intersperse (B.singleton ',') $ map render args) <> B.singleton ']'

data Target
  = Player Name
  | TS TargetSelector
    deriving (Eq, Ord, Read, Show, Data, Typeable, Generic)

instance Render Target where
  render (Player name) = render name
  render (TS ts) = render ts

data Item
  = IronShovel
  | IronPickaxe
    deriving (Eq, Ord, Read, Show, Data, Typeable, Generic)
{-
instance Render Item where
  render item =
    case item of
     IronShovel -> "minecraft:iron_shovel"
     IronPickaxe -> "minecraft:iron_pickaxe"
-}
instance TextShow Item where
  showb item =
    case item of
     IronShovel  -> "minecraft:iron_shovel"
     IronPickaxe -> "minecraft:iron_pickaxe"

data Difficulty
  = Peaceful
  | Easy
  | Normal
  | Hard
    deriving (Eq, Ord, Read, Show, Data, Typeable, Generic)

instance ToNBTContents Difficulty where
  toNBTContents difficulty =
    ByteTag $ case difficulty of
      Peaceful -> 0
      Easy     -> 1
      Normal   -> 2
      Hard     -> 3

-- | time of day values
sunrise, midday, sunset, midnight, nextday :: Int64
sunrise  =     0
midday   =  6000
sunset   = 12000
midnight = 18000
nextday  = 24000

data Dimension
  = Nether
  | Overworld
  | End
  deriving (Eq, Ord, Read, Show, Data, Typeable, Generic)

instance ToNBTContents Dimension where
  toNBTContents dimension =
    IntTag $ case dimension of
      Nether    -> (-1)
      Overworld -> 0
      End       -> 1

data XYZ
  = XYZ -- ^ absolute position
  { _x :: Int32
  , _y :: Int32
  , _z :: Int32
  }
  | RXYZ -- ^ relative position
    { _x :: Int32
    , _y :: Int32
    , _z :: Int32
    }
  deriving (Eq, Ord, Read, Show, Data, Typeable, Generic)
makeLenses ''XYZ

data Attribute = Attribute
    deriving (Eq, Ord, Read, Show, Data, Typeable, Generic)

instance ToNBTContents Attribute where
  toNBTContents = error "ToNBTContent Attribute not implemented."
