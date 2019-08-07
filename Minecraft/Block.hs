{-# language DeriveDataTypeable #-}
{-# language DeriveGeneric #-}
{-# language OverloadedStrings #-}
module Minecraft.Block where

import Data.Bimap (Bimap, fromList)
import Data.Data
import Data.Text (Text)
import Data.Monoid ((<>))
import qualified Data.Text as Text
import GHC.Generics

data NamespacedId = NamespacedId { namespace :: Text, name :: Text }
  deriving (Eq, Ord, Read, Show, Data, Generic)

formatNamespacedId :: NamespacedId -> Text
formatNamespacedId (NamespacedId ns n) = ns <> Text.singleton ':' <> n

data Block
  = AcaciaButton
  | AcaciaDoor
  | AcaciaFenceGate
  | AcaciaFence
  | AcaciaLeaves
  | AcaciaLog
  | AcaciaPlanks
  | AcaciaPressurePlate
  | AcaciaSapling
  | AcaciaSign
  | AcaciaSlab
  | AcaciaStairs
  | AcaciaTrapdoor
  | AcaciaWallSign
  | AcaciaWood
  | ActivatorRail
  | Air
  | Allium
  | Andesite
  | AndesiteSlab
  | AndesiteStairs
  | AndesiteWall
  | Anvil
  | AttachedMelonStem
  | AttachedPumpkinStem
  | AzureBluet
  | Bamboo
  | BambooSapling
  | Barrel
  | Barrier
  | Beacon
  | Bedrock
  | Beetroots
  | Bell
  | BirchButton
  | BirchDoor
  | BirchFenceGate
  | BirchFence
  | BirchLeaves
  | BirchLog
  | BirchPlanks
  | BirchPressurePlate
  | BirchSapling
  | BirchSign
  | BirchSlab
  | BirchStairs
  | BirchTrapdoor
  | BirchWallSign
  | BirchWood
  | BlackBanner
  | BlackBed
  | BlackCarpet
  | BlackConcretePowder
  | BlackConcrete
  | BlackGlazedTerracotta
  | BlackShulkerBox
  | BlackStainedGlass
  | BlackStainedGlassPane
  | BlackTerracotta
  | BlackWallBanner
  | BlackWool
  | BlastFurnace
  | BlueBanner
  | BlueBed
  | BlueCarpet
  | BlueConcretePowder
  | BlueConcrete
  | BlueGlazedTerracotta
  | BlueIce
  | BlueOrchid
  | BlueShulkerBox
  | BlueStainedGlass
  | BlueStainedGlassPane
  | BlueTerracotta
  | BlueWallBanner
  | BlueWool
  | BoneBlock
  | Bookshelf
  | BrainCoral
  | BrainCoralBlock
  | BrainCoralFan
  | BrainCoralWallFan
  | BrewingStand
  | BrickSlab
  | BrickStairs
  | BrickWall
  | Bricks
  | BrownBanner
  | BrownBed
  | BrownCarpet
  | BrownConcretePowder
  | BrownConcrete
  | BrownGlazedTerracotta
  | BrownMushroomBlock
  | BrownMushroom
  | BrownShulkerBox
  | BrownStainedGlass
  | BrownStainedGlassPane
  | BrownTerracotta
  | BrownWallBanner
  | BrownWool
  | BubbleColumn
  | BubbleCoral
  | BubbleCoralBlock
  | BubbleCoralFan
  | BubbleCoralWallFan
  | Cactus
  | Cake
  | Campfire
  | Carrots
  | CartographyTable
  | CarvedPumpkin
  | Cauldron
  | CaveAir
  | ChainCommandBlock
  | Chest
  | ChippedAnvil
  | ChiseledQuartzBlock
  | ChiseledRedSandstone
  | ChiseledSandstone
  | ChiseledStoneBricks
  | ChorusFlower
  | ChorusPlant
  | Clay
  | CoalBlock
  | CoalOre
  | CoarseDirt
  | Cobblestone
  | CobblestoneSlab
  | CobblestoneStairs
  | CobblestoneWall
  | Cobweb
  | Cocoa
  | CommandBlock
  | Comparator
  | Composter
  | Conduit
  | Cornflower
  | CrackedStoneBricks
  | CraftingTable
  | CreeperHead
  | CreeperWallHead
  | CutRedSandstone
  | CutRedSandstoneSlab
  | CutSandstone
  | CutSandstoneSlab
  | CyanBanner
  | CyanBed
  | CyanCarpet
  | CyanConcretePowder
  | CyanConcrete
  | CyanGlazedTerracotta
  | CyanShulkerBox
  | CyanStainedGlass
  | CyanStainedGlassPane
  | CyanTerracotta
  | CyanWallBanner
  | CyanWool
  | DamagedAnvil
  | Dandelion
  | DarkOakButton
  | DarkOakDoor
  | DarkOakFenceGate
  | DarkOakFence
  | DarkOakLeaves
  | DarkOakLog
  | DarkOakPlanks
  | DarkOakPressurePlate
  | DarkOakSapling
  | DarkOakSign
  | DarkOakSlab
  | DarkOakStairs
  | DarkOakTrapdoor
  | DarkOakWallSign
  | DarkOakWood
  | DarkPrismarine
  | DarkPrismarineSlab
  | DarkPrismarineStairs
  | DaylightDetector
  | DeadBrainCoral
  | DeadBrainCoralBlock
  | DeadBrainCoralFan
  | DeadBrainCoralWallFan
  | DeadBubbleCoral
  | DeadBubbleCoralBlock
  | DeadBubbleCoralFan
  | DeadBubbleCoralWallFan
  | DeadBush
  | DeadFireCoral
  | DeadFireCoralBlock
  | DeadFireCoralFan
  | DeadFireCoralWallFan
  | DeadHornCoral
  | DeadHornCoralBlock
  | DeadHornCoralFan
  | DeadHornCoralWallFan
  | DeadTubeCoral
  | DeadTubeCoralBlock
  | DeadTubeCoralFan
  | DeadTubeCoralWallFan
  | DetectorRail
  | DiamondBlock
  | DiamondOre
  | Diorite
  | DioriteSlab
  | DioriteStairs
  | DioriteWall
  | Dirt
  | Dispenser
  | DragonEgg
  | DragonHead
  | DragonWallHead
  | DriedKelpBlock
  | Dropper
  | EmeraldBlock
  | EmeraldOre
  | EnchantingTable
  | EndGateway
  | EndPortalFrame
  | EndPortal
  | EndRod
  | EndStone
  | EndStoneBrickSlab
  | EndStoneBrickStairs
  | EndStoneBrickWall
  | EndStoneBricks
  | EnderChest
  | Farmland
  | Fern
  | Fire
  | FireCoral
  | FireCoralBlock
  | FireCoralFan
  | FireCoralWallFan
  | FletchingTable
  | FlowerPot
  | FrostedIce
  | Furnace
  | Glass
  | GlassPane
  | Glowstone
  | GoldBlock
  | GoldOre
  | Granite
  | GraniteSlab
  | GraniteStairs
  | GraniteWall
  | GrassBlock
  | GrassPath
  | Grass
  | Gravel
  | GrayBanner
  | GrayBed
  | GrayCarpet
  | GrayConcretePowder
  | GrayConcrete
  | GrayGlazedTerracotta
  | GrayShulkerBox
  | GrayStainedGlass
  | GrayStainedGlassPane
  | GrayTerracotta
  | GrayWallBanner
  | GrayWool
  | GreenBanner
  | GreenBed
  | GreenCarpet
  | GreenConcretePowder
  | GreenConcrete
  | GreenGlazedTerracotta
  | GreenShulkerBox
  | GreenStainedGlass
  | GreenStainedGlassPane
  | GreenTerracotta
  | GreenWallBanner
  | GreenWool
  | Grindstone
  | HayBlock
  | HeavyWeightedPressurePlate
  | Hopper
  | HornCoral
  | HornCoralBlock
  | HornCoralFan
  | HornCoralWallFan
  | Ice
  | InfestedChiseledStoneBricks
  | InfestedCobblestone
  | InfestedCrackedStoneBricks
  | InfestedMossyStoneBricks
  | InfestedStone
  | InfestedStoneBricks
  | IronBars
  | IronDoor
  | IronBlock
  | IronOre
  | IronTrapdoor
  | JackOLantern
  | Jigsaw
  | Jukebox
  | JungleButton
  | JungleDoor
  | JungleFenceGate
  | JungleFence
  | JungleLeaves
  | JungleLog
  | JunglePlanks
  | JunglePressurePlate
  | JungleSapling
  | JungleSign
  | JungleSlab
  | JungleStairs
  | JungleTrapdoor
  | JungleWallSign
  | JungleWood
  | Kelp
  | KelpPlant
  | Ladder
  | Lantern
  | LapisBlock
  | LapisOre
  | LargeFern
  | Lava
  | Lectern
  | Lever
  | LightBlueBanner
  | LightBlueBed
  | LightBlueCarpet
  | LightBlueConcretePowder
  | LightBlueConcrete
  | LightBlueGlazedTerracotta
  | LightBlueShulkerBox
  | LightBlueStainedGlass
  | LightBlueStainedGlassPane
  | LightBlueTerracotta
  | LightBlueWallBanner
  | LightBlueWool
  | LightGrayBanner
  | LightGrayBed
  | LightGrayCarpet
  | LightGrayConcretePowder
  | LightGrayConcrete
  | LightGrayGlazedTerracotta
  | LightGrayShulkerBox
  | LightGrayStainedGlass
  | LightGrayStainedGlassPane
  | LightGrayTerracotta
  | LightGrayWallBanner
  | LightGrayWool
  | LightWeightedPressurePlate
  | Lilac
  | LilyPad
  | LilyOfTheValley
  | LimeBanner
  | LimeBed
  | LimeCarpet
  | LimeConcretePowder
  | LimeConcrete
  | LimeGlazedTerracotta
  | LimeShulkerBox
  | LimeStainedGlass
  | LimeStainedGlassPane
  | LimeTerracotta
  | LimeWallBanner
  | LimeWool
  | Loom
  | MagentaBanner
  | MagentaBed
  | MagentaCarpet
  | MagentaConcretePowder
  | MagentaConcrete
  | MagentaGlazedTerracotta
  | MagentaShulkerBox
  | MagentaStainedGlass
  | MagentaStainedGlassPane
  | MagentaTerracotta
  | MagentaWallBanner
  | MagentaWool
  | MagmaBlock
  | Melon
  | MelonStem
  | MossyCobblestone
  | MossyCobblestoneSlab
  | MossyCobblestoneStairs
  | MossyCobblestoneWall
  | MossyStoneBrickSlab
  | MossyStoneBrickStairs
  | MossyStoneBrickWall
  | MossyStoneBricks
  | MovingPiston
  | MushroomStem
  | Mycelium
  | NetherBrickFence
  | NetherBrickSlab
  | NetherBrickStairs
  | NetherBrickWall
  | NetherBricks
  | NetherPortal
  | NetherQuartzOre
  | NetherWartBlock
  | NetherWart
  | Netherrack
  | NoteBlock
  | OakButton
  | OakDoor
  | OakFenceGate
  | OakFence
  | OakLeaves
  | OakLog
  | OakPlanks
  | OakPressurePlate
  | OakSapling
  | OakSign
  | OakSlab
  | OakStairs
  | OakTrapdoor
  | WallSign
  | OakWallSign
  | OakWood
  | Observer
  | Obsidian
  | OrangeBanner
  | OrangeBed
  | OrangeCarpet
  | OrangeConcretePowder
  | OrangeConcrete
  | OrangeGlazedTerracotta
  | OrangeShulkerBox
  | OrangeStainedGlass
  | OrangeStainedGlassPane
  | OrangeTerracotta
  | OrangeTulip
  | OrangeWallBanner
  | OrangeWool
  | OxeyeDaisy
  | PackedIce
  | Peony
  | PetrifiedOakSlab
  | PinkBanner
  | PinkBed
  | PinkCarpet
  | PinkConcretePowder
  | PinkConcrete
  | PinkGlazedTerracotta
  | PinkShulkerBox
  | PinkStainedGlass
  | PinkStainedGlassPane
  | PinkTerracotta
  | PinkTulip
  | PinkWallBanner
  | PinkWool
  | PistonHead
  | Piston
  | PlayerHead
  | PlayerWallHead
  | Podzol
  | PolishedAndesite
  | PolishedAndesiteSlab
  | PolishedAndesiteStairs
  | PolishedDiorite
  | PolishedDioriteSlab
  | PolishedDioriteStairs
  | PolishedGranite
  | PolishedGraniteSlab
  | PolishedGraniteStairs
  | Poppy
  | Potatoes
  | PottedAcaciaSapling
  | PottedAllium
  | PottedAzureBluet
  | PottedBamboo
  | PottedBirchSapling
  | PottedBlueOrchid
  | PottedBrownMushroom
  | PottedCactus
  | PottedCornflower
  | PottedDandelion
  | PottedDarkOakSapling
  | PottedDeadBush
  | PottedFern
  | PottedJungleSapling
  | PottedLilyOfTheValley
  | PottedOakSapling
  | PottedOrangeTulip
  | PottedOxeyeDaisy
  | PottedPinkTulip
  | PottedPoppy
  | PottedRedMushroom
  | PottedRedTulip
  | PottedSpruceSapling
  | PottedWhiteTulip
  | PottedWitherRose
  | PoweredRail
  | Prismarine
  | PrismarineBrickSlab
  | PrismarineBrickStairs
  | PrismarineBricks
  | PrismarineSlab
  | PrismarineStairs
  | PrismarineWall
  | Pumpkin
  | PumpkinStem
  | PurpleBanner
  | PurpleBed
  | PurpleCarpet
  | PurpleConcretePowder
  | PurpleConcrete
  | PurpleGlazedTerracotta
  | PurpleShulkerBox
  | PurpleStainedGlass
  | PurpleStainedGlassPane
  | PurpleTerracotta
  | PurpleWallBanner
  | PurpleWool
  | PurpurBlock
  | PurpurPillar
  | PurpurSlab
  | PurpurStairs
  | QuartzBlock
  | QuartzPillar
  | QuartzSlab
  | QuartzStairs
  | Rail
  | RedBanner
  | RedBed
  | RedCarpet
  | RedConcretePowder
  | RedConcrete
  | RedGlazedTerracotta
  | RedMushroomBlock
  | RedMushroom
  | RedNetherBrickSlab
  | RedNetherBrickStairs
  | RedNetherBrickWall
  | RedNetherBricks
  | RedSand
  | RedSandstone
  | RedSandstoneSlab
  | RedSandstoneStairs
  | RedSandstoneWall
  | RedShulkerBox
  | RedStainedGlass
  | RedStainedGlassPane
  | RedTerracotta
  | RedTulip
  | RedWallBanner
  | RedWool
  | RedstoneBlock
  | RedstoneLamp
  | RedstoneOre
  | RedstoneTorch
  | RedstoneWallTorch
  | RedstoneWire
  | Repeater
  | RepeatingCommandBlock
  | RoseBush
  | Sand
  | Sandstone
  | SandstoneSlab
  | SandstoneStairs
  | SandstoneWall
  | Scaffolding
  | SeaLantern
  | SeaPickle
  | Seagrass
  | ShulkerBox
  | SkeletonSkull
  | SkeletonWallSkull
  | SlimeBlock
  | SmithingTable
  | Smoker
  | SmoothQuartz
  | SmoothQuartzSlab
  | SmoothQuartzStairs
  | SmoothRedSandstone
  | SmoothRedSandstoneSlab
  | SmoothRedSandstoneStairs
  | SmoothSandstone
  | SmoothSandstoneSlab
  | SmoothSandstoneStairs
  | SmoothStone
  | SmoothStoneSlab
  | SnowBlock
  | Snow
  | SoulSand
  | Spawner
  | Sponge
  | SpruceButton
  | SpruceDoor
  | SpruceFenceGate
  | SpruceFence
  | SpruceLeaves
  | SpruceLog
  | SprucePlanks
  | SprucePressurePlate
  | SpruceSapling
  | SpruceSign
  | SpruceSlab
  | SpruceStairs
  | SpruceTrapdoor
  | SpruceWallSign
  | SpruceWood
  | StickyPiston
  | Stone
  | StoneBrickSlab
  | StoneBrickStairs
  | StoneBrickWall
  | StoneBricks
  | StoneButton
  | StonePressurePlate
  | StoneSlab
  | StoneStairs
  | Stonecutter
  | StrippedAcaciaLog
  | StrippedAcaciaWood
  | StrippedBirchLog
  | StrippedBirchWood
  | StrippedDarkOakLog
  | StrippedDarkOakWood
  | StrippedJungleLog
  | StrippedJungleWood
  | StrippedOakLog
  | StrippedOakWood
  | StrippedSpruceLog
  | StrippedSpruceWood
  | StructureBlock
  | StructureVoid
  | SugarCane
  | Sunflower
  | SweetBerryBush
  | Tnt
  | TallGrass
  | TallSeagrass
  | Terracotta
  | Torch
  | TrappedChest
  | TripwireHook
  | Tripwire
  | TubeCoral
  | TubeCoralBlock
  | TubeCoralFan
  | TubeCoralWallFan
  | TurtleEgg
  | Vine
  | VoidAir
  | WallTorch
  | Water
  | WetSponge
  | Wheat
  | WhiteBanner
  | WhiteBed
  | WhiteCarpet
  | WhiteConcretePowder
  | WhiteConcrete
  | WhiteGlazedTerracotta
  | WhiteShulkerBox
  | WhiteStainedGlass
  | WhiteStainedGlassPane
  | WhiteTerracotta
  | WhiteTulip
  | WhiteWallBanner
  | WhiteWool
  | WitherRose
  | WitherSkeletonSkull
  | WitherSkeletonWallSkull
  | YellowBanner
  | YellowBed
  | YellowCarpet
  | YellowConcretePowder
  | YellowConcrete
  | YellowGlazedTerracotta
  | YellowShulkerBox
  | YellowStainedGlass
  | YellowStainedGlassPane
  | YellowTerracotta
  | YellowWallBanner
  | YellowWool
  | ZombieHead
  | ZombieWallHead
  deriving (Eq, Ord, Read, Show, Enum, Data, Generic)

blockNames :: Bimap Block NamespacedId
blockNames = fromList [
    (AcaciaButton, NamespacedId "minecraft" "acacia_button")
  , (AcaciaDoor, NamespacedId "minecraft" "acacia_door")
  , (AcaciaFenceGate, NamespacedId "minecraft" "acacia_fence_gate")
  , (AcaciaFence, NamespacedId "minecraft" "acacia_fence")
  , (AcaciaLeaves, NamespacedId "minecraft" "acacia_leaves")
  , (AcaciaLog, NamespacedId "minecraft" "acacia_log")
  , (AcaciaPlanks, NamespacedId "minecraft" "acacia_planks")
  , (AcaciaPressurePlate, NamespacedId "minecraft" "acacia_pressure_plate")
  , (AcaciaSapling, NamespacedId "minecraft" "acacia_sapling")
  , (AcaciaSign, NamespacedId "minecraft" "acacia_sign")
  , (AcaciaSlab, NamespacedId "minecraft" "acacia_slab")
  , (AcaciaStairs, NamespacedId "minecraft" "acacia_stairs")
  , (AcaciaTrapdoor, NamespacedId "minecraft" "acacia_trapdoor")
  , (AcaciaWallSign, NamespacedId "minecraft" "acacia_wall_sign")
  , (AcaciaWood, NamespacedId "minecraft" "acacia_wood")
  , (ActivatorRail, NamespacedId "minecraft" "activator_rail")
  , (Air, NamespacedId "minecraft" "air")
  , (Allium, NamespacedId "minecraft" "allium")
  , (Andesite, NamespacedId "minecraft" "andesite")
  , (AndesiteSlab, NamespacedId "minecraft" "andesite_slab")
  , (AndesiteStairs, NamespacedId "minecraft" "andesite_stairs")
  , (AndesiteWall, NamespacedId "minecraft" "andesite_wall")
  , (Anvil, NamespacedId "minecraft" "anvil")
  , (AttachedMelonStem, NamespacedId "minecraft" "attached_melon_stem")
  , (AttachedPumpkinStem, NamespacedId "minecraft" "attached_pumpkin_stem")
  , (AzureBluet, NamespacedId "minecraft" "azure_bluet")
  , (Bamboo, NamespacedId "minecraft" "bamboo")
  , (BambooSapling, NamespacedId "minecraft" "bamboo_sapling")
  , (Barrel, NamespacedId "minecraft" "barrel")
  , (Barrier, NamespacedId "minecraft" "barrier")
  , (Beacon, NamespacedId "minecraft" "beacon")
  , (Bedrock, NamespacedId "minecraft" "bedrock")
  , (Beetroots, NamespacedId "minecraft" "beetroots")
  , (Bell, NamespacedId "minecraft" "bell")
  , (BirchButton, NamespacedId "minecraft" "birch_button")
  , (BirchDoor, NamespacedId "minecraft" "birch_door")
  , (BirchFenceGate, NamespacedId "minecraft" "birch_fence_gate")
  , (BirchFence, NamespacedId "minecraft" "birch_fence")
  , (BirchLeaves, NamespacedId "minecraft" "birch_leaves")
  , (BirchLog, NamespacedId "minecraft" "birch_log")
  , (BirchPlanks, NamespacedId "minecraft" "birch_planks")
  , (BirchPressurePlate, NamespacedId "minecraft" "birch_pressure_plate")
  , (BirchSapling, NamespacedId "minecraft" "birch_sapling")
  , (BirchSign, NamespacedId "minecraft" "birch_sign")
  , (BirchSlab, NamespacedId "minecraft" "birch_slab")
  , (BirchStairs, NamespacedId "minecraft" "birch_stairs")
  , (BirchTrapdoor, NamespacedId "minecraft" "birch_trapdoor")
  , (BirchWallSign, NamespacedId "minecraft" "birch_wall_sign")
  , (BirchWood, NamespacedId "minecraft" "birch_wood")
  , (BlackBanner, NamespacedId "minecraft" "black_banner")
  , (BlackBed, NamespacedId "minecraft" "black_bed")
  , (BlackCarpet, NamespacedId "minecraft" "black_carpet")
  , (BlackConcretePowder, NamespacedId "minecraft" "black_concrete_powder")
  , (BlackConcrete, NamespacedId "minecraft" "black_concrete")
  , (BlackGlazedTerracotta, NamespacedId "minecraft" "black_glazed_terracotta")
  , (BlackShulkerBox, NamespacedId "minecraft" "black_shulker_box")
  , (BlackStainedGlass, NamespacedId "minecraft" "black_stained_glass")
  , (BlackStainedGlassPane, NamespacedId "minecraft" "black_stained_glass_pane")
  , (BlackTerracotta, NamespacedId "minecraft" "black_terracotta")
  , (BlackWallBanner, NamespacedId "minecraft" "black_wall_banner")
  , (BlackWool, NamespacedId "minecraft" "black_wool")
  , (BlastFurnace, NamespacedId "minecraft" "blast_furnace")
  , (BlueBanner, NamespacedId "minecraft" "blue_banner")
  , (BlueBed, NamespacedId "minecraft" "blue_bed")
  , (BlueCarpet, NamespacedId "minecraft" "blue_carpet")
  , (BlueConcretePowder, NamespacedId "minecraft" "blue_concrete_powder")
  , (BlueConcrete, NamespacedId "minecraft" "blue_concrete")
  , (BlueGlazedTerracotta, NamespacedId "minecraft" "blue_glazed_terracotta")
  , (BlueIce, NamespacedId "minecraft" "blue_ice")
  , (BlueOrchid, NamespacedId "minecraft" "blue_orchid")
  , (BlueShulkerBox, NamespacedId "minecraft" "blue_shulker_box")
  , (BlueStainedGlass, NamespacedId "minecraft" "blue_stained_glass")
  , (BlueStainedGlassPane, NamespacedId "minecraft" "blue_stained_glass_pane")
  , (BlueTerracotta, NamespacedId "minecraft" "blue_terracotta")
  , (BlueWallBanner, NamespacedId "minecraft" "blue_wall_banner")
  , (BlueWool, NamespacedId "minecraft" "blue_wool")
  , (BoneBlock, NamespacedId "minecraft" "bone_block")
  , (Bookshelf, NamespacedId "minecraft" "bookshelf")
  , (BrainCoral, NamespacedId "minecraft" "brain_coral")
  , (BrainCoralBlock, NamespacedId "minecraft" "brain_coral_block")
  , (BrainCoralFan, NamespacedId "minecraft" "brain_coral_fan")
  , (BrainCoralWallFan, NamespacedId "minecraft" "brain_coral_wall_fan")
  , (BrewingStand, NamespacedId "minecraft" "brewing_stand")
  , (BrickSlab, NamespacedId "minecraft" "brick_slab")
  , (BrickStairs, NamespacedId "minecraft" "brick_stairs")
  , (BrickWall, NamespacedId "minecraft" "brick_wall")
  , (Bricks, NamespacedId "minecraft" "bricks")
  , (BrownBanner, NamespacedId "minecraft" "brown_banner")
  , (BrownBed, NamespacedId "minecraft" "brown_bed")
  , (BrownCarpet, NamespacedId "minecraft" "brown_carpet")
  , (BrownConcretePowder, NamespacedId "minecraft" "brown_concrete_powder")
  , (BrownConcrete, NamespacedId "minecraft" "brown_concrete")
  , (BrownGlazedTerracotta, NamespacedId "minecraft" "brown_glazed_terracotta")
  , (BrownMushroomBlock, NamespacedId "minecraft" "brown_mushroom_block")
  , (BrownMushroom, NamespacedId "minecraft" "brown_mushroom")
  , (BrownShulkerBox, NamespacedId "minecraft" "brown_shulker_box")
  , (BrownStainedGlass, NamespacedId "minecraft" "brown_stained_glass")
  , (BrownStainedGlassPane, NamespacedId "minecraft" "brown_stained_glass_pane")
  , (BrownTerracotta, NamespacedId "minecraft" "brown_terracotta")
  , (BrownWallBanner, NamespacedId "minecraft" "brown_wall_banner")
  , (BrownWool, NamespacedId "minecraft" "brown_wool")
  , (BubbleColumn, NamespacedId "minecraft" "bubble_column")
  , (BubbleCoral, NamespacedId "minecraft" "bubble_coral")
  , (BubbleCoralBlock, NamespacedId "minecraft" "bubble_coral_block")
  , (BubbleCoralFan, NamespacedId "minecraft" "bubble_coral_fan")
  , (BubbleCoralWallFan, NamespacedId "minecraft" "bubble_coral_wall_fan")
  , (Cactus, NamespacedId "minecraft" "cactus")
  , (Cake, NamespacedId "minecraft" "cake")
  , (Campfire, NamespacedId "minecraft" "campfire")
  , (Carrots, NamespacedId "minecraft" "carrots")
  , (CartographyTable, NamespacedId "minecraft" "cartography_table")
  , (CarvedPumpkin, NamespacedId "minecraft" "carved_pumpkin")
  , (Cauldron, NamespacedId "minecraft" "cauldron")
  , (CaveAir, NamespacedId "minecraft" "cave_air")
  , (ChainCommandBlock, NamespacedId "minecraft" "chain_command_block")
  , (Chest, NamespacedId "minecraft" "chest")
  , (ChippedAnvil, NamespacedId "minecraft" "chipped_anvil")
  , (ChiseledQuartzBlock, NamespacedId "minecraft" "chiseled_quartz_block")
  , (ChiseledRedSandstone, NamespacedId "minecraft" "chiseled_red_sandstone")
  , (ChiseledSandstone, NamespacedId "minecraft" "chiseled_sandstone")
  , (ChiseledStoneBricks, NamespacedId "minecraft" "chiseled_stone_bricks")
  , (ChorusFlower, NamespacedId "minecraft" "chorus_flower")
  , (ChorusPlant, NamespacedId "minecraft" "chorus_plant")
  , (Clay, NamespacedId "minecraft" "clay")
  , (CoalBlock, NamespacedId "minecraft" "coal_block")
  , (CoalOre, NamespacedId "minecraft" "coal_ore")
  , (CoarseDirt, NamespacedId "minecraft" "coarse_dirt")
  , (Cobblestone, NamespacedId "minecraft" "cobblestone")
  , (CobblestoneSlab, NamespacedId "minecraft" "cobblestone_slab")
  , (CobblestoneStairs, NamespacedId "minecraft" "cobblestone_stairs")
  , (CobblestoneWall, NamespacedId "minecraft" "cobblestone_wall")
  , (Cobweb, NamespacedId "minecraft" "cobweb")
  , (Cocoa, NamespacedId "minecraft" "cocoa")
  , (CommandBlock, NamespacedId "minecraft" "command_block")
  , (Comparator, NamespacedId "minecraft" "comparator")
  , (Composter, NamespacedId "minecraft" "composter")
  , (Conduit, NamespacedId "minecraft" "conduit")
  , (Cornflower, NamespacedId "minecraft" "cornflower")
  , (CrackedStoneBricks, NamespacedId "minecraft" "cracked_stone_bricks")
  , (CraftingTable, NamespacedId "minecraft" "crafting_table")
  , (CreeperHead, NamespacedId "minecraft" "creeper_head")
  , (CreeperWallHead, NamespacedId "minecraft" "creeper_wall_head")
  , (CutRedSandstone, NamespacedId "minecraft" "cut_red_sandstone")
  , (CutRedSandstoneSlab, NamespacedId "minecraft" "cut_red_sandstone_slab")
  , (CutSandstone, NamespacedId "minecraft" "cut_sandstone")
  , (CutSandstoneSlab, NamespacedId "minecraft" "cut_sandstone_slab")
  , (CyanBanner, NamespacedId "minecraft" "cyan_banner")
  , (CyanBed, NamespacedId "minecraft" "cyan_bed")
  , (CyanCarpet, NamespacedId "minecraft" "cyan_carpet")
  , (CyanConcretePowder, NamespacedId "minecraft" "cyan_concrete_powder")
  , (CyanConcrete, NamespacedId "minecraft" "cyan_concrete")
  , (CyanGlazedTerracotta, NamespacedId "minecraft" "cyan_glazed_terracotta")
  , (CyanShulkerBox, NamespacedId "minecraft" "cyan_shulker_box")
  , (CyanStainedGlass, NamespacedId "minecraft" "cyan_stained_glass")
  , (CyanStainedGlassPane, NamespacedId "minecraft" "cyan_stained_glass_pane")
  , (CyanTerracotta, NamespacedId "minecraft" "cyan_terracotta")
  , (CyanWallBanner, NamespacedId "minecraft" "cyan_wall_banner")
  , (CyanWool, NamespacedId "minecraft" "cyan_wool")
  , (DamagedAnvil, NamespacedId "minecraft" "damaged_anvil")
  , (Dandelion, NamespacedId "minecraft" "dandelion")
  , (DarkOakButton, NamespacedId "minecraft" "dark_oak_button")
  , (DarkOakDoor, NamespacedId "minecraft" "dark_oak_door")
  , (DarkOakFenceGate, NamespacedId "minecraft" "dark_oak_fence_gate")
  , (DarkOakFence, NamespacedId "minecraft" "dark_oak_fence")
  , (DarkOakLeaves, NamespacedId "minecraft" "dark_oak_leaves")
  , (DarkOakLog, NamespacedId "minecraft" "dark_oak_log")
  , (DarkOakPlanks, NamespacedId "minecraft" "dark_oak_planks")
  , (DarkOakPressurePlate, NamespacedId "minecraft" "dark_oak_pressure_plate")
  , (DarkOakSapling, NamespacedId "minecraft" "dark_oak_sapling")
  , (DarkOakSign, NamespacedId "minecraft" "dark_oak_sign")
  , (DarkOakSlab, NamespacedId "minecraft" "dark_oak_slab")
  , (DarkOakStairs, NamespacedId "minecraft" "dark_oak_stairs")
  , (DarkOakTrapdoor, NamespacedId "minecraft" "dark_oak_trapdoor")
  , (DarkOakWallSign, NamespacedId "minecraft" "dark_oak_wall_sign")
  , (DarkOakWood, NamespacedId "minecraft" "dark_oak_wood")
  , (DarkPrismarine, NamespacedId "minecraft" "dark_prismarine")
  , (DarkPrismarineSlab, NamespacedId "minecraft" "dark_prismarine_slab")
  , (DarkPrismarineStairs, NamespacedId "minecraft" "dark_prismarine_stairs")
  , (DaylightDetector, NamespacedId "minecraft" "daylight_detector")
  , (DeadBrainCoral, NamespacedId "minecraft" "dead_brain_coral")
  , (DeadBrainCoralBlock, NamespacedId "minecraft" "dead_brain_coral_block")
  , (DeadBrainCoralFan, NamespacedId "minecraft" "dead_brain_coral_fan")
  , (DeadBrainCoralWallFan, NamespacedId "minecraft" "dead_brain_coral_wall_fan")
  , (DeadBubbleCoral, NamespacedId "minecraft" "dead_bubble_coral")
  , (DeadBubbleCoralBlock, NamespacedId "minecraft" "dead_bubble_coral_block")
  , (DeadBubbleCoralFan, NamespacedId "minecraft" "dead_bubble_coral_fan")
  , (DeadBubbleCoralWallFan, NamespacedId "minecraft" "dead_bubble_coral_wall_fan")
  , (DeadBush, NamespacedId "minecraft" "dead_bush")
  , (DeadFireCoral, NamespacedId "minecraft" "dead_fire_coral")
  , (DeadFireCoralBlock, NamespacedId "minecraft" "dead_fire_coral_block")
  , (DeadFireCoralFan, NamespacedId "minecraft" "dead_fire_coral_fan")
  , (DeadFireCoralWallFan, NamespacedId "minecraft" "dead_fire_coral_wall_fan")
  , (DeadHornCoral, NamespacedId "minecraft" "dead_horn_coral")
  , (DeadHornCoralBlock, NamespacedId "minecraft" "dead_horn_coral_block")
  , (DeadHornCoralFan, NamespacedId "minecraft" "dead_horn_coral_fan")
  , (DeadHornCoralWallFan, NamespacedId "minecraft" "dead_horn_coral_wall_fan")
  , (DeadTubeCoral, NamespacedId "minecraft" "dead_tube_coral")
  , (DeadTubeCoralBlock, NamespacedId "minecraft" "dead_tube_coral_block")
  , (DeadTubeCoralFan, NamespacedId "minecraft" "dead_tube_coral_fan")
  , (DeadTubeCoralWallFan, NamespacedId "minecraft" "dead_tube_coral_wall_fan")
  , (DetectorRail, NamespacedId "minecraft" "detector_rail")
  , (DiamondBlock, NamespacedId "minecraft" "diamond_block")
  , (DiamondOre, NamespacedId "minecraft" "diamond_ore")
  , (Diorite, NamespacedId "minecraft" "diorite")
  , (DioriteSlab, NamespacedId "minecraft" "diorite_slab")
  , (DioriteStairs, NamespacedId "minecraft" "diorite_stairs")
  , (DioriteWall, NamespacedId "minecraft" "diorite_wall")
  , (Dirt, NamespacedId "minecraft" "dirt")
  , (Dispenser, NamespacedId "minecraft" "dispenser")
  , (DragonEgg, NamespacedId "minecraft" "dragon_egg")
  , (DragonHead, NamespacedId "minecraft" "dragon_head")
  , (DragonWallHead, NamespacedId "minecraft" "dragon_wall_head")
  , (DriedKelpBlock, NamespacedId "minecraft" "dried_kelp_block")
  , (Dropper, NamespacedId "minecraft" "dropper")
  , (EmeraldBlock, NamespacedId "minecraft" "emerald_block")
  , (EmeraldOre, NamespacedId "minecraft" "emerald_ore")
  , (EnchantingTable, NamespacedId "minecraft" "enchanting_table")
  , (EndGateway, NamespacedId "minecraft" "end_gateway")
  , (EndPortalFrame, NamespacedId "minecraft" "end_portal_frame")
  , (EndPortal, NamespacedId "minecraft" "end_portal")
  , (EndRod, NamespacedId "minecraft" "end_rod")
  , (EndStone, NamespacedId "minecraft" "end_stone")
  , (EndStoneBrickSlab, NamespacedId "minecraft" "end_stone_brick_slab")
  , (EndStoneBrickStairs, NamespacedId "minecraft" "end_stone_brick_stairs")
  , (EndStoneBrickWall, NamespacedId "minecraft" "end_stone_brick_wall")
  , (EndStoneBricks, NamespacedId "minecraft" "end_stone_bricks")
  , (EnderChest, NamespacedId "minecraft" "ender_chest")
  , (Farmland, NamespacedId "minecraft" "farmland")
  , (Fern, NamespacedId "minecraft" "fern")
  , (Fire, NamespacedId "minecraft" "fire")
  , (FireCoral, NamespacedId "minecraft" "fire_coral")
  , (FireCoralBlock, NamespacedId "minecraft" "fire_coral_block")
  , (FireCoralFan, NamespacedId "minecraft" "fire_coral_fan")
  , (FireCoralWallFan, NamespacedId "minecraft" "fire_coral_wall_fan")
  , (FletchingTable, NamespacedId "minecraft" "fletching_table")
  , (FlowerPot, NamespacedId "minecraft" "flower_pot")
  , (FrostedIce, NamespacedId "minecraft" "frosted_ice")
  , (Furnace, NamespacedId "minecraft" "furnace")
  , (Glass, NamespacedId "minecraft" "glass")
  , (GlassPane, NamespacedId "minecraft" "glass_pane")
  , (Glowstone, NamespacedId "minecraft" "glowstone")
  , (GoldBlock, NamespacedId "minecraft" "gold_block")
  , (GoldOre, NamespacedId "minecraft" "gold_ore")
  , (Granite, NamespacedId "minecraft" "granite")
  , (GraniteSlab, NamespacedId "minecraft" "granite_slab")
  , (GraniteStairs, NamespacedId "minecraft" "granite_stairs")
  , (GraniteWall, NamespacedId "minecraft" "granite_wall")
  , (GrassBlock, NamespacedId "minecraft" "grass_block")
  , (GrassPath, NamespacedId "minecraft" "grass_path")
  , (Grass, NamespacedId "minecraft" "grass")
  , (Gravel, NamespacedId "minecraft" "gravel")
  , (GrayBanner, NamespacedId "minecraft" "gray_banner")
  , (GrayBed, NamespacedId "minecraft" "gray_bed")
  , (GrayCarpet, NamespacedId "minecraft" "gray_carpet")
  , (GrayConcretePowder, NamespacedId "minecraft" "gray_concrete_powder")
  , (GrayConcrete, NamespacedId "minecraft" "gray_concrete")
  , (GrayGlazedTerracotta, NamespacedId "minecraft" "gray_glazed_terracotta")
  , (GrayShulkerBox, NamespacedId "minecraft" "gray_shulker_box")
  , (GrayStainedGlass, NamespacedId "minecraft" "gray_stained_glass")
  , (GrayStainedGlassPane, NamespacedId "minecraft" "gray_stained_glass_pane")
  , (GrayTerracotta, NamespacedId "minecraft" "gray_terracotta")
  , (GrayWallBanner, NamespacedId "minecraft" "gray_wall_banner")
  , (GrayWool, NamespacedId "minecraft" "gray_wool")
  , (GreenBanner, NamespacedId "minecraft" "green_banner")
  , (GreenBed, NamespacedId "minecraft" "green_bed")
  , (GreenCarpet, NamespacedId "minecraft" "green_carpet")
  , (GreenConcretePowder, NamespacedId "minecraft" "green_concrete_powder")
  , (GreenConcrete, NamespacedId "minecraft" "green_concrete")
  , (GreenGlazedTerracotta, NamespacedId "minecraft" "green_glazed_terracotta")
  , (GreenShulkerBox, NamespacedId "minecraft" "green_shulker_box")
  , (GreenStainedGlass, NamespacedId "minecraft" "green_stained_glass")
  , (GreenStainedGlassPane, NamespacedId "minecraft" "green_stained_glass_pane")
  , (GreenTerracotta, NamespacedId "minecraft" "green_terracotta")
  , (GreenWallBanner, NamespacedId "minecraft" "green_wall_banner")
  , (GreenWool, NamespacedId "minecraft" "green_wool")
  , (Grindstone, NamespacedId "minecraft" "grindstone")
  , (HayBlock, NamespacedId "minecraft" "hay_block")
  , (HeavyWeightedPressurePlate, NamespacedId "minecraft" "heavy_weighted_pressure_plate")
  , (Hopper, NamespacedId "minecraft" "hopper")
  , (HornCoral, NamespacedId "minecraft" "horn_coral")
  , (HornCoralBlock, NamespacedId "minecraft" "horn_coral_block")
  , (HornCoralFan, NamespacedId "minecraft" "horn_coral_fan")
  , (HornCoralWallFan, NamespacedId "minecraft" "horn_coral_wall_fan")
  , (Ice, NamespacedId "minecraft" "ice")
  , (InfestedChiseledStoneBricks, NamespacedId "minecraft" "infested_chiseled_stone_bricks")
  , (InfestedCobblestone, NamespacedId "minecraft" "infested_cobblestone")
  , (InfestedCrackedStoneBricks, NamespacedId "minecraft" "infested_cracked_stone_bricks")
  , (InfestedMossyStoneBricks, NamespacedId "minecraft" "infested_mossy_stone_bricks")
  , (InfestedStone, NamespacedId "minecraft" "infested_stone")
  , (InfestedStoneBricks, NamespacedId "minecraft" "infested_stone_bricks")
  , (IronBars, NamespacedId "minecraft" "iron_bars")
  , (IronDoor, NamespacedId "minecraft" "iron_door")
  , (IronBlock, NamespacedId "minecraft" "iron_block")
  , (IronOre, NamespacedId "minecraft" "iron_ore")
  , (IronTrapdoor, NamespacedId "minecraft" "iron_trapdoor")
  , (JackOLantern, NamespacedId "minecraft" "jack_o_lantern")
  , (Jigsaw, NamespacedId "minecraft" "jigsaw")
  , (Jukebox, NamespacedId "minecraft" "jukebox")
  , (JungleButton, NamespacedId "minecraft" "jungle_button")
  , (JungleDoor, NamespacedId "minecraft" "jungle_door")
  , (JungleFenceGate, NamespacedId "minecraft" "jungle_fence_gate")
  , (JungleFence, NamespacedId "minecraft" "jungle_fence")
  , (JungleLeaves, NamespacedId "minecraft" "jungle_leaves")
  , (JungleLog, NamespacedId "minecraft" "jungle_log")
  , (JunglePlanks, NamespacedId "minecraft" "jungle_planks")
  , (JunglePressurePlate, NamespacedId "minecraft" "jungle_pressure_plate")
  , (JungleSapling, NamespacedId "minecraft" "jungle_sapling")
  , (JungleSign, NamespacedId "minecraft" "jungle_sign")
  , (JungleSlab, NamespacedId "minecraft" "jungle_slab")
  , (JungleStairs, NamespacedId "minecraft" "jungle_stairs")
  , (JungleTrapdoor, NamespacedId "minecraft" "jungle_trapdoor")
  , (JungleWallSign, NamespacedId "minecraft" "jungle_wall_sign")
  , (JungleWood, NamespacedId "minecraft" "jungle_wood")
  , (Kelp, NamespacedId "minecraft" "kelp")
  , (KelpPlant, NamespacedId "minecraft" "kelp_plant")
  , (Ladder, NamespacedId "minecraft" "ladder")
  , (Lantern, NamespacedId "minecraft" "lantern")
  , (LapisBlock, NamespacedId "minecraft" "lapis_block")
  , (LapisOre, NamespacedId "minecraft" "lapis_ore")
  , (LargeFern, NamespacedId "minecraft" "large_fern")
  , (Lava, NamespacedId "minecraft" "lava")
  , (Lectern, NamespacedId "minecraft" "lectern")
  , (Lever, NamespacedId "minecraft" "lever")
  , (LightBlueBanner, NamespacedId "minecraft" "light_blue_banner")
  , (LightBlueBed, NamespacedId "minecraft" "light_blue_bed")
  , (LightBlueCarpet, NamespacedId "minecraft" "light_blue_carpet")
  , (LightBlueConcretePowder, NamespacedId "minecraft" "light_blue_concrete_powder")
  , (LightBlueConcrete, NamespacedId "minecraft" "light_blue_concrete")
  , (LightBlueGlazedTerracotta, NamespacedId "minecraft" "light_blue_glazed_terracotta")
  , (LightBlueShulkerBox, NamespacedId "minecraft" "light_blue_shulker_box")
  , (LightBlueStainedGlass, NamespacedId "minecraft" "light_blue_stained_glass")
  , (LightBlueStainedGlassPane, NamespacedId "minecraft" "light_blue_stained_glass_pane")
  , (LightBlueTerracotta, NamespacedId "minecraft" "light_blue_terracotta")
  , (LightBlueWallBanner, NamespacedId "minecraft" "light_blue_wall_banner")
  , (LightBlueWool, NamespacedId "minecraft" "light_blue_wool")
  , (LightGrayBanner, NamespacedId "minecraft" "light_gray_banner")
  , (LightGrayBed, NamespacedId "minecraft" "light_gray_bed")
  , (LightGrayCarpet, NamespacedId "minecraft" "light_gray_carpet")
  , (LightGrayConcretePowder, NamespacedId "minecraft" "light_gray_concrete_powder")
  , (LightGrayConcrete, NamespacedId "minecraft" "light_gray_concrete")
  , (LightGrayGlazedTerracotta, NamespacedId "minecraft" "light_gray_glazed_terracotta")
  , (LightGrayShulkerBox, NamespacedId "minecraft" "light_gray_shulker_box")
  , (LightGrayStainedGlass, NamespacedId "minecraft" "light_gray_stained_glass")
  , (LightGrayStainedGlassPane, NamespacedId "minecraft" "light_gray_stained_glass_pane")
  , (LightGrayTerracotta, NamespacedId "minecraft" "light_gray_terracotta")
  , (LightGrayWallBanner, NamespacedId "minecraft" "light_gray_wall_banner")
  , (LightGrayWool, NamespacedId "minecraft" "light_gray_wool")
  , (LightWeightedPressurePlate, NamespacedId "minecraft" "light_weighted_pressure_plate")
  , (Lilac, NamespacedId "minecraft" "lilac")
  , (LilyPad, NamespacedId "minecraft" "lily_pad")
  , (LilyOfTheValley, NamespacedId "minecraft" "lily_of_the_valley")
  , (LimeBanner, NamespacedId "minecraft" "lime_banner")
  , (LimeBed, NamespacedId "minecraft" "lime_bed")
  , (LimeCarpet, NamespacedId "minecraft" "lime_carpet")
  , (LimeConcretePowder, NamespacedId "minecraft" "lime_concrete_powder")
  , (LimeConcrete, NamespacedId "minecraft" "lime_concrete")
  , (LimeGlazedTerracotta, NamespacedId "minecraft" "lime_glazed_terracotta")
  , (LimeShulkerBox, NamespacedId "minecraft" "lime_shulker_box")
  , (LimeStainedGlass, NamespacedId "minecraft" "lime_stained_glass")
  , (LimeStainedGlassPane, NamespacedId "minecraft" "lime_stained_glass_pane")
  , (LimeTerracotta, NamespacedId "minecraft" "lime_terracotta")
  , (LimeWallBanner, NamespacedId "minecraft" "lime_wall_banner")
  , (LimeWool, NamespacedId "minecraft" "lime_wool")
  , (Loom, NamespacedId "minecraft" "loom")
  , (MagentaBanner, NamespacedId "minecraft" "magenta_banner")
  , (MagentaBed, NamespacedId "minecraft" "magenta_bed")
  , (MagentaCarpet, NamespacedId "minecraft" "magenta_carpet")
  , (MagentaConcretePowder, NamespacedId "minecraft" "magenta_concrete_powder")
  , (MagentaConcrete, NamespacedId "minecraft" "magenta_concrete")
  , (MagentaGlazedTerracotta, NamespacedId "minecraft" "magenta_glazed_terracotta")
  , (MagentaShulkerBox, NamespacedId "minecraft" "magenta_shulker_box")
  , (MagentaStainedGlass, NamespacedId "minecraft" "magenta_stained_glass")
  , (MagentaStainedGlassPane, NamespacedId "minecraft" "magenta_stained_glass_pane")
  , (MagentaTerracotta, NamespacedId "minecraft" "magenta_terracotta")
  , (MagentaWallBanner, NamespacedId "minecraft" "magenta_wall_banner")
  , (MagentaWool, NamespacedId "minecraft" "magenta_wool")
  , (MagmaBlock, NamespacedId "minecraft" "magma_block")
  , (Melon, NamespacedId "minecraft" "melon")
  , (MelonStem, NamespacedId "minecraft" "melon_stem")
  , (MossyCobblestone, NamespacedId "minecraft" "mossy_cobblestone")
  , (MossyCobblestoneSlab, NamespacedId "minecraft" "mossy_cobblestone_slab")
  , (MossyCobblestoneStairs, NamespacedId "minecraft" "mossy_cobblestone_stairs")
  , (MossyCobblestoneWall, NamespacedId "minecraft" "mossy_cobblestone_wall")
  , (MossyStoneBrickSlab, NamespacedId "minecraft" "mossy_stone_brick_slab")
  , (MossyStoneBrickStairs, NamespacedId "minecraft" "mossy_stone_brick_stairs")
  , (MossyStoneBrickWall, NamespacedId "minecraft" "mossy_stone_brick_wall")
  , (MossyStoneBricks, NamespacedId "minecraft" "mossy_stone_bricks")
  , (MovingPiston, NamespacedId "minecraft" "moving_piston")
  , (MushroomStem, NamespacedId "minecraft" "mushroom_stem")
  , (Mycelium, NamespacedId "minecraft" "mycelium")
  , (NetherBrickFence, NamespacedId "minecraft" "nether_brick_fence")
  , (NetherBrickSlab, NamespacedId "minecraft" "nether_brick_slab")
  , (NetherBrickStairs, NamespacedId "minecraft" "nether_brick_stairs")
  , (NetherBrickWall, NamespacedId "minecraft" "nether_brick_wall")
  , (NetherBricks, NamespacedId "minecraft" "nether_bricks")
  , (NetherPortal, NamespacedId "minecraft" "nether_portal")
  , (NetherQuartzOre, NamespacedId "minecraft" "nether_quartz_ore")
  , (NetherWartBlock, NamespacedId "minecraft" "nether_wart_block")
  , (NetherWart, NamespacedId "minecraft" "nether_wart")
  , (Netherrack, NamespacedId "minecraft" "netherrack")
  , (NoteBlock, NamespacedId "minecraft" "note_block")
  , (OakButton, NamespacedId "minecraft" "oak_button")
  , (OakDoor, NamespacedId "minecraft" "oak_door")
  , (OakFenceGate, NamespacedId "minecraft" "oak_fence_gate")
  , (OakFence, NamespacedId "minecraft" "oak_fence")
  , (OakLeaves, NamespacedId "minecraft" "oak_leaves")
  , (OakLog, NamespacedId "minecraft" "oak_log")
  , (OakPlanks, NamespacedId "minecraft" "oak_planks")
  , (OakPressurePlate, NamespacedId "minecraft" "oak_pressure_plate")
  , (OakSapling, NamespacedId "minecraft" "oak_sapling")
  , (OakSign, NamespacedId "minecraft" "oak_sign")
  , (OakSlab, NamespacedId "minecraft" "oak_slab")
  , (OakStairs, NamespacedId "minecraft" "oak_stairs")
  , (OakTrapdoor, NamespacedId "minecraft" "oak_trapdoor")
  , (WallSign, NamespacedId "minecraft" "wall_sign")
  , (OakWallSign, NamespacedId "minecraft" "oak_wall_sign")
  , (OakWood, NamespacedId "minecraft" "oak_wood")
  , (Observer, NamespacedId "minecraft" "observer")
  , (Obsidian, NamespacedId "minecraft" "obsidian")
  , (OrangeBanner, NamespacedId "minecraft" "orange_banner")
  , (OrangeBed, NamespacedId "minecraft" "orange_bed")
  , (OrangeCarpet, NamespacedId "minecraft" "orange_carpet")
  , (OrangeConcretePowder, NamespacedId "minecraft" "orange_concrete_powder")
  , (OrangeConcrete, NamespacedId "minecraft" "orange_concrete")
  , (OrangeGlazedTerracotta, NamespacedId "minecraft" "orange_glazed_terracotta")
  , (OrangeShulkerBox, NamespacedId "minecraft" "orange_shulker_box")
  , (OrangeStainedGlass, NamespacedId "minecraft" "orange_stained_glass")
  , (OrangeStainedGlassPane, NamespacedId "minecraft" "orange_stained_glass_pane")
  , (OrangeTerracotta, NamespacedId "minecraft" "orange_terracotta")
  , (OrangeTulip, NamespacedId "minecraft" "orange_tulip")
  , (OrangeWallBanner, NamespacedId "minecraft" "orange_wall_banner")
  , (OrangeWool, NamespacedId "minecraft" "orange_wool")
  , (OxeyeDaisy, NamespacedId "minecraft" "oxeye_daisy")
  , (PackedIce, NamespacedId "minecraft" "packed_ice")
  , (Peony, NamespacedId "minecraft" "peony")
  , (PetrifiedOakSlab, NamespacedId "minecraft" "petrified_oak_slab")
  , (PinkBanner, NamespacedId "minecraft" "pink_banner")
  , (PinkBed, NamespacedId "minecraft" "pink_bed")
  , (PinkCarpet, NamespacedId "minecraft" "pink_carpet")
  , (PinkConcretePowder, NamespacedId "minecraft" "pink_concrete_powder")
  , (PinkConcrete, NamespacedId "minecraft" "pink_concrete")
  , (PinkGlazedTerracotta, NamespacedId "minecraft" "pink_glazed_terracotta")
  , (PinkShulkerBox, NamespacedId "minecraft" "pink_shulker_box")
  , (PinkStainedGlass, NamespacedId "minecraft" "pink_stained_glass")
  , (PinkStainedGlassPane, NamespacedId "minecraft" "pink_stained_glass_pane")
  , (PinkTerracotta, NamespacedId "minecraft" "pink_terracotta")
  , (PinkTulip, NamespacedId "minecraft" "pink_tulip")
  , (PinkWallBanner, NamespacedId "minecraft" "pink_wall_banner")
  , (PinkWool, NamespacedId "minecraft" "pink_wool")
  , (PistonHead, NamespacedId "minecraft" "piston_head")
  , (Piston, NamespacedId "minecraft" "piston")
  , (PlayerHead, NamespacedId "minecraft" "player_head")
  , (PlayerWallHead, NamespacedId "minecraft" "player_wall_head")
  , (Podzol, NamespacedId "minecraft" "podzol")
  , (PolishedAndesite, NamespacedId "minecraft" "polished_andesite")
  , (PolishedAndesiteSlab, NamespacedId "minecraft" "polished_andesite_slab")
  , (PolishedAndesiteStairs, NamespacedId "minecraft" "polished_andesite_stairs")
  , (PolishedDiorite, NamespacedId "minecraft" "polished_diorite")
  , (PolishedDioriteSlab, NamespacedId "minecraft" "polished_diorite_slab")
  , (PolishedDioriteStairs, NamespacedId "minecraft" "polished_diorite_stairs")
  , (PolishedGranite, NamespacedId "minecraft" "polished_granite")
  , (PolishedGraniteSlab, NamespacedId "minecraft" "polished_granite_slab")
  , (PolishedGraniteStairs, NamespacedId "minecraft" "polished_granite_stairs")
  , (Poppy, NamespacedId "minecraft" "poppy")
  , (Potatoes, NamespacedId "minecraft" "potatoes")
  , (PottedAcaciaSapling, NamespacedId "minecraft" "potted_acacia_sapling")
  , (PottedAllium, NamespacedId "minecraft" "potted_allium")
  , (PottedAzureBluet, NamespacedId "minecraft" "potted_azure_bluet")
  , (PottedBamboo, NamespacedId "minecraft" "potted_bamboo")
  , (PottedBirchSapling, NamespacedId "minecraft" "potted_birch_sapling")
  , (PottedBlueOrchid, NamespacedId "minecraft" "potted_blue_orchid")
  , (PottedBrownMushroom, NamespacedId "minecraft" "potted_brown_mushroom")
  , (PottedCactus, NamespacedId "minecraft" "potted_cactus")
  , (PottedCornflower, NamespacedId "minecraft" "potted_cornflower")
  , (PottedDandelion, NamespacedId "minecraft" "potted_dandelion")
  , (PottedDarkOakSapling, NamespacedId "minecraft" "potted_dark_oak_sapling")
  , (PottedDeadBush, NamespacedId "minecraft" "potted_dead_bush")
  , (PottedFern, NamespacedId "minecraft" "potted_fern")
  , (PottedJungleSapling, NamespacedId "minecraft" "potted_jungle_sapling")
  , (PottedLilyOfTheValley, NamespacedId "minecraft" "potted_lily_of_the_valley")
  , (PottedOakSapling, NamespacedId "minecraft" "potted_oak_sapling")
  , (PottedOrangeTulip, NamespacedId "minecraft" "potted_orange_tulip")
  , (PottedOxeyeDaisy, NamespacedId "minecraft" "potted_oxeye_daisy")
  , (PottedPinkTulip, NamespacedId "minecraft" "potted_pink_tulip")
  , (PottedPoppy, NamespacedId "minecraft" "potted_poppy")
  , (PottedRedMushroom, NamespacedId "minecraft" "potted_red_mushroom")
  , (PottedRedTulip, NamespacedId "minecraft" "potted_red_tulip")
  , (PottedSpruceSapling, NamespacedId "minecraft" "potted_spruce_sapling")
  , (PottedWhiteTulip, NamespacedId "minecraft" "potted_white_tulip")
  , (PottedWitherRose, NamespacedId "minecraft" "potted_wither_rose")
  , (PoweredRail, NamespacedId "minecraft" "powered_rail")
  , (Prismarine, NamespacedId "minecraft" "prismarine")
  , (PrismarineBrickSlab, NamespacedId "minecraft" "prismarine_brick_slab")
  , (PrismarineBrickStairs, NamespacedId "minecraft" "prismarine_brick_stairs")
  , (PrismarineBricks, NamespacedId "minecraft" "prismarine_bricks")
  , (PrismarineSlab, NamespacedId "minecraft" "prismarine_slab")
  , (PrismarineStairs, NamespacedId "minecraft" "prismarine_stairs")
  , (PrismarineWall, NamespacedId "minecraft" "prismarine_wall")
  , (Pumpkin, NamespacedId "minecraft" "pumpkin")
  , (PumpkinStem, NamespacedId "minecraft" "pumpkin_stem")
  , (PurpleBanner, NamespacedId "minecraft" "purple_banner")
  , (PurpleBed, NamespacedId "minecraft" "purple_bed")
  , (PurpleCarpet, NamespacedId "minecraft" "purple_carpet")
  , (PurpleConcretePowder, NamespacedId "minecraft" "purple_concrete_powder")
  , (PurpleConcrete, NamespacedId "minecraft" "purple_concrete")
  , (PurpleGlazedTerracotta, NamespacedId "minecraft" "purple_glazed_terracotta")
  , (PurpleShulkerBox, NamespacedId "minecraft" "purple_shulker_box")
  , (PurpleStainedGlass, NamespacedId "minecraft" "purple_stained_glass")
  , (PurpleStainedGlassPane, NamespacedId "minecraft" "purple_stained_glass_pane")
  , (PurpleTerracotta, NamespacedId "minecraft" "purple_terracotta")
  , (PurpleWallBanner, NamespacedId "minecraft" "purple_wall_banner")
  , (PurpleWool, NamespacedId "minecraft" "purple_wool")
  , (PurpurBlock, NamespacedId "minecraft" "purpur_block")
  , (PurpurPillar, NamespacedId "minecraft" "purpur_pillar")
  , (PurpurSlab, NamespacedId "minecraft" "purpur_slab")
  , (PurpurStairs, NamespacedId "minecraft" "purpur_stairs")
  , (QuartzBlock, NamespacedId "minecraft" "quartz_block")
  , (QuartzPillar, NamespacedId "minecraft" "quartz_pillar")
  , (QuartzSlab, NamespacedId "minecraft" "quartz_slab")
  , (QuartzStairs, NamespacedId "minecraft" "quartz_stairs")
  , (Rail, NamespacedId "minecraft" "rail")
  , (RedBanner, NamespacedId "minecraft" "red_banner")
  , (RedBed, NamespacedId "minecraft" "red_bed")
  , (RedCarpet, NamespacedId "minecraft" "red_carpet")
  , (RedConcretePowder, NamespacedId "minecraft" "red_concrete_powder")
  , (RedConcrete, NamespacedId "minecraft" "red_concrete")
  , (RedGlazedTerracotta, NamespacedId "minecraft" "red_glazed_terracotta")
  , (RedMushroomBlock, NamespacedId "minecraft" "red_mushroom_block")
  , (RedMushroom, NamespacedId "minecraft" "red_mushroom")
  , (RedNetherBrickSlab, NamespacedId "minecraft" "red_nether_brick_slab")
  , (RedNetherBrickStairs, NamespacedId "minecraft" "red_nether_brick_stairs")
  , (RedNetherBrickWall, NamespacedId "minecraft" "red_nether_brick_wall")
  , (RedNetherBricks, NamespacedId "minecraft" "red_nether_bricks")
  , (RedSand, NamespacedId "minecraft" "red_sand")
  , (RedSandstone, NamespacedId "minecraft" "red_sandstone")
  , (RedSandstoneSlab, NamespacedId "minecraft" "red_sandstone_slab")
  , (RedSandstoneStairs, NamespacedId "minecraft" "red_sandstone_stairs")
  , (RedSandstoneWall, NamespacedId "minecraft" "red_sandstone_wall")
  , (RedShulkerBox, NamespacedId "minecraft" "red_shulker_box")
  , (RedStainedGlass, NamespacedId "minecraft" "red_stained_glass")
  , (RedStainedGlassPane, NamespacedId "minecraft" "red_stained_glass_pane")
  , (RedTerracotta, NamespacedId "minecraft" "red_terracotta")
  , (RedTulip, NamespacedId "minecraft" "red_tulip")
  , (RedWallBanner, NamespacedId "minecraft" "red_wall_banner")
  , (RedWool, NamespacedId "minecraft" "red_wool")
  , (RedstoneBlock, NamespacedId "minecraft" "redstone_block")
  , (RedstoneLamp, NamespacedId "minecraft" "redstone_lamp")
  , (RedstoneOre, NamespacedId "minecraft" "redstone_ore")
  , (RedstoneTorch, NamespacedId "minecraft" "redstone_torch")
  , (RedstoneWallTorch, NamespacedId "minecraft" "redstone_wall_torch")
  , (RedstoneWire, NamespacedId "minecraft" "redstone_wire")
  , (Repeater, NamespacedId "minecraft" "repeater")
  , (RepeatingCommandBlock, NamespacedId "minecraft" "repeating_command_block")
  , (RoseBush, NamespacedId "minecraft" "rose_bush")
  , (Sand, NamespacedId "minecraft" "sand")
  , (Sandstone, NamespacedId "minecraft" "sandstone")
  , (SandstoneSlab, NamespacedId "minecraft" "sandstone_slab")
  , (SandstoneStairs, NamespacedId "minecraft" "sandstone_stairs")
  , (SandstoneWall, NamespacedId "minecraft" "sandstone_wall")
  , (Scaffolding, NamespacedId "minecraft" "scaffolding")
  , (SeaLantern, NamespacedId "minecraft" "sea_lantern")
  , (SeaPickle, NamespacedId "minecraft" "sea_pickle")
  , (Seagrass, NamespacedId "minecraft" "seagrass")
  , (ShulkerBox, NamespacedId "minecraft" "shulker_box")
  , (SkeletonSkull, NamespacedId "minecraft" "skeleton_skull")
  , (SkeletonWallSkull, NamespacedId "minecraft" "skeleton_wall_skull")
  , (SlimeBlock, NamespacedId "minecraft" "slime_block")
  , (SmithingTable, NamespacedId "minecraft" "smithing_table")
  , (Smoker, NamespacedId "minecraft" "smoker")
  , (SmoothQuartz, NamespacedId "minecraft" "smooth_quartz")
  , (SmoothQuartzSlab, NamespacedId "minecraft" "smooth_quartz_slab")
  , (SmoothQuartzStairs, NamespacedId "minecraft" "smooth_quartz_stairs")
  , (SmoothRedSandstone, NamespacedId "minecraft" "smooth_red_sandstone")
  , (SmoothRedSandstoneSlab, NamespacedId "minecraft" "smooth_red_sandstone_slab")
  , (SmoothRedSandstoneStairs, NamespacedId "minecraft" "smooth_red_sandstone_stairs")
  , (SmoothSandstone, NamespacedId "minecraft" "smooth_sandstone")
  , (SmoothSandstoneSlab, NamespacedId "minecraft" "smooth_sandstone_slab")
  , (SmoothSandstoneStairs, NamespacedId "minecraft" "smooth_sandstone_stairs")
  , (SmoothStone, NamespacedId "minecraft" "smooth_stone")
  , (SmoothStoneSlab, NamespacedId "minecraft" "smooth_stone_slab")
  , (SnowBlock, NamespacedId "minecraft" "snow_block")
  , (Snow, NamespacedId "minecraft" "snow")
  , (SoulSand, NamespacedId "minecraft" "soul_sand")
  , (Spawner, NamespacedId "minecraft" "spawner")
  , (Sponge, NamespacedId "minecraft" "sponge")
  , (SpruceButton, NamespacedId "minecraft" "spruce_button")
  , (SpruceDoor, NamespacedId "minecraft" "spruce_door")
  , (SpruceFenceGate, NamespacedId "minecraft" "spruce_fence_gate")
  , (SpruceFence, NamespacedId "minecraft" "spruce_fence")
  , (SpruceLeaves, NamespacedId "minecraft" "spruce_leaves")
  , (SpruceLog, NamespacedId "minecraft" "spruce_log")
  , (SprucePlanks, NamespacedId "minecraft" "spruce_planks")
  , (SprucePressurePlate, NamespacedId "minecraft" "spruce_pressure_plate")
  , (SpruceSapling, NamespacedId "minecraft" "spruce_sapling")
  , (SpruceSign, NamespacedId "minecraft" "spruce_sign")
  , (SpruceSlab, NamespacedId "minecraft" "spruce_slab")
  , (SpruceStairs, NamespacedId "minecraft" "spruce_stairs")
  , (SpruceTrapdoor, NamespacedId "minecraft" "spruce_trapdoor")
  , (SpruceWallSign, NamespacedId "minecraft" "spruce_wall_sign")
  , (SpruceWood, NamespacedId "minecraft" "spruce_wood")
  , (StickyPiston, NamespacedId "minecraft" "sticky_piston")
  , (Stone, NamespacedId "minecraft" "stone")
  , (StoneBrickSlab, NamespacedId "minecraft" "stone_brick_slab")
  , (StoneBrickStairs, NamespacedId "minecraft" "stone_brick_stairs")
  , (StoneBrickWall, NamespacedId "minecraft" "stone_brick_wall")
  , (StoneBricks, NamespacedId "minecraft" "stone_bricks")
  , (StoneButton, NamespacedId "minecraft" "stone_button")
  , (StonePressurePlate, NamespacedId "minecraft" "stone_pressure_plate")
  , (StoneSlab, NamespacedId "minecraft" "stone_slab")
  , (StoneStairs, NamespacedId "minecraft" "stone_stairs")
  , (Stonecutter, NamespacedId "minecraft" "stonecutter")
  , (StrippedAcaciaLog, NamespacedId "minecraft" "stripped_acacia_log")
  , (StrippedAcaciaWood, NamespacedId "minecraft" "stripped_acacia_wood")
  , (StrippedBirchLog, NamespacedId "minecraft" "stripped_birch_log")
  , (StrippedBirchWood, NamespacedId "minecraft" "stripped_birch_wood")
  , (StrippedDarkOakLog, NamespacedId "minecraft" "stripped_dark_oak_log")
  , (StrippedDarkOakWood, NamespacedId "minecraft" "stripped_dark_oak_wood")
  , (StrippedJungleLog, NamespacedId "minecraft" "stripped_jungle_log")
  , (StrippedJungleWood, NamespacedId "minecraft" "stripped_jungle_wood")
  , (StrippedOakLog, NamespacedId "minecraft" "stripped_oak_log")
  , (StrippedOakWood, NamespacedId "minecraft" "stripped_oak_wood")
  , (StrippedSpruceLog, NamespacedId "minecraft" "stripped_spruce_log")
  , (StrippedSpruceWood, NamespacedId "minecraft" "stripped_spruce_wood")
  , (StructureBlock, NamespacedId "minecraft" "structure_block")
  , (StructureVoid, NamespacedId "minecraft" "structure_void")
  , (SugarCane, NamespacedId "minecraft" "sugar_cane")
  , (Sunflower, NamespacedId "minecraft" "sunflower")
  , (SweetBerryBush, NamespacedId "minecraft" "sweet_berry_bush")
  , (Tnt, NamespacedId "minecraft" "tnt")
  , (TallGrass, NamespacedId "minecraft" "tall_grass")
  , (TallSeagrass, NamespacedId "minecraft" "tall_seagrass")
  , (Terracotta, NamespacedId "minecraft" "terracotta")
  , (Torch, NamespacedId "minecraft" "torch")
  , (TrappedChest, NamespacedId "minecraft" "trapped_chest")
  , (TripwireHook, NamespacedId "minecraft" "tripwire_hook")
  , (Tripwire, NamespacedId "minecraft" "tripwire")
  , (TubeCoral, NamespacedId "minecraft" "tube_coral")
  , (TubeCoralBlock, NamespacedId "minecraft" "tube_coral_block")
  , (TubeCoralFan, NamespacedId "minecraft" "tube_coral_fan")
  , (TubeCoralWallFan, NamespacedId "minecraft" "tube_coral_wall_fan")
  , (TurtleEgg, NamespacedId "minecraft" "turtle_egg")
  , (Vine, NamespacedId "minecraft" "vine")
  , (VoidAir, NamespacedId "minecraft" "void_air")
  , (WallTorch, NamespacedId "minecraft" "wall_torch")
  , (Water, NamespacedId "minecraft" "water")
  , (WetSponge, NamespacedId "minecraft" "wet_sponge")
  , (Wheat, NamespacedId "minecraft" "wheat")
  , (WhiteBanner, NamespacedId "minecraft" "white_banner")
  , (WhiteBed, NamespacedId "minecraft" "white_bed")
  , (WhiteCarpet, NamespacedId "minecraft" "white_carpet")
  , (WhiteConcretePowder, NamespacedId "minecraft" "white_concrete_powder")
  , (WhiteConcrete, NamespacedId "minecraft" "white_concrete")
  , (WhiteGlazedTerracotta, NamespacedId "minecraft" "white_glazed_terracotta")
  , (WhiteShulkerBox, NamespacedId "minecraft" "white_shulker_box")
  , (WhiteStainedGlass, NamespacedId "minecraft" "white_stained_glass")
  , (WhiteStainedGlassPane, NamespacedId "minecraft" "white_stained_glass_pane")
  , (WhiteTerracotta, NamespacedId "minecraft" "white_terracotta")
  , (WhiteTulip, NamespacedId "minecraft" "white_tulip")
  , (WhiteWallBanner, NamespacedId "minecraft" "white_wall_banner")
  , (WhiteWool, NamespacedId "minecraft" "white_wool")
  , (WitherRose, NamespacedId "minecraft" "wither_rose")
  , (WitherSkeletonSkull, NamespacedId "minecraft" "wither_skeleton_skull")
  , (WitherSkeletonWallSkull, NamespacedId "minecraft" "wither_skeleton_wall_skull")
  , (YellowBanner, NamespacedId "minecraft" "yellow_banner")
  , (YellowBed, NamespacedId "minecraft" "yellow_bed")
  , (YellowCarpet, NamespacedId "minecraft" "yellow_carpet")
  , (YellowConcretePowder, NamespacedId "minecraft" "yellow_concrete_powder")
  , (YellowConcrete, NamespacedId "minecraft" "yellow_concrete")
  , (YellowGlazedTerracotta, NamespacedId "minecraft" "yellow_glazed_terracotta")
  , (YellowShulkerBox, NamespacedId "minecraft" "yellow_shulker_box")
  , (YellowStainedGlass, NamespacedId "minecraft" "yellow_stained_glass")
  , (YellowStainedGlassPane, NamespacedId "minecraft" "yellow_stained_glass_pane")
  , (YellowTerracotta, NamespacedId "minecraft" "yellow_terracotta")
  , (YellowWallBanner, NamespacedId "minecraft" "yellow_wall_banner")
  , (YellowWool, NamespacedId "minecraft" "yellow_wool")
  , (ZombieHead, NamespacedId "minecraft" "zombie_head")
  , (ZombieWallHead, NamespacedId "minecraft" "zombie_wall_head")
  ]
