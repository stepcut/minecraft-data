{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
-- https://www.digminecraft.com/game_commands/setblock_command.php
-- Using 1.13/1.14 syntax
module Minecraft.Command where

import Data.Data (Data)
import qualified Data.Bimap as Bimap
import Data.List (intersperse)
import Data.Typeable (Typeable)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Monoid ((<>), mconcat, mempty)
import Data.NBT
import Data.Word (Word, Word8)
import Data.Text (Text)
import Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as B
import GHC.Generics
import Minecraft.Core
import Minecraft.Block
import TextShow (showb)

data OldBlockHandling
  = Destroy
  | Keep
  | Replace
    deriving (Eq, Show)

data DataValue
  = Facing CardinalDirection
  | BlockType BlockType
    deriving (Eq, Ord, Read, Show)

instance Render DataValue where
  render (Facing dir) = "facing=" <> render dir
  render (BlockType bt) = "type=" <> render bt

instance Render [DataValue] where
  render [] = mempty
  render dvs = B.singleton '[' <> mconcat (intersperse (B.fromText ", ") $ map render dvs) <> B.singleton ']'

data Command
  = Give Target Item (Maybe Int6) (Maybe Int64) (Maybe NBT)
  | SetBlock XYZ Block [DataValue] (Maybe OldBlockHandling)
    deriving (Eq, Show)

(<+>) :: Builder -> Builder -> Builder
a <+> b = a <> B.singleton ' ' <> b

(<?>) :: (Render b) => Builder -> Maybe b -> Builder
a <?> Nothing  = a
a <?> (Just b) = a <+> (render b)

instance Render NBT where
  render nbt = error "Render NBT not implemented."

instance Render PosKind where
  render Abs   = mempty
  render Tilda = B.singleton '~'
  render Caret = B.singleton '^'

instance Render Pos where
  render (Pos k v) = render k <> (showb v)

instance Render XYZ where
  render (XYZ x y z) = (render x) <+>  (render y) <+> (render z)

instance Render OldBlockHandling where
  render Destroy = "destroy"
  render Keep    = "keep"
  render Replace = "replace"

instance Render NamespacedId where
  render (NamespacedId ns n) = B.fromText ns <> B.singleton ':' <> B.fromText n

instance Render Block where
  render blk =
    case Bimap.lookup blk blockNames of
      (Just n) -> render n


instance Render Command where
  render command =
    case command of
     (Give target item Nothing Nothing Nothing) ->
       "give" <+> render target <+> showb item
     (Give target item mAmount mData mDataTag) ->
       "give" <+> render target <+> showb item <+> (maybe "1" render mAmount) <+> (maybe "0" render mData) <?> mDataTag
     (SetBlock xyz blk dvs mOldBlockHandling) ->
       "setblock" <+> render xyz <+> render blk <> render dvs <?> mOldBlockHandling
