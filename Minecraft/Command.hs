{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
-- https://www.digminecraft.com/game_commands/setblock_command.php
-- Using 1.13/1.14 syntax
module Minecraft.Command where

import Data.Data (Data)
import qualified Data.Bimap as Bimap
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

data Command
  = Give Target Item (Maybe Int6) (Maybe Int64) (Maybe NBT)
  | SetBlock XYZ Block (Maybe OldBlockHandling)
    deriving (Eq, Show)

(<+>) :: Builder -> Builder -> Builder
a <+> b = a <> B.singleton ' ' <> b

(<?>) :: (Render b) => Builder -> Maybe b -> Builder
a <?> Nothing  = a
a <?> (Just b) = a <+> (render b)

instance Render NBT where
  render nbt = error "Render NBT not implemented."

-- FIXME: add support for ~
instance Render XYZ where
  render (XYZ x y z) = (showb x) <+>  (showb y) <+> (showb z)
  render (RXYZ x y z) = B.singleton '~' <> (showb x) <+> B.singleton '~' <> (showb y) <+> B.singleton '~' <> (showb z)

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
     (SetBlock xyz blk mOldBlockHandling) ->
       "setblock" <+> render xyz <+> render blk <?> mOldBlockHandling
