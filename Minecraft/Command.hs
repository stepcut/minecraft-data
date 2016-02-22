{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Minecraft.Command where

import Data.Data (Data)
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

data Command
  = Give Target Item (Maybe Int6) (Maybe Int64) (Maybe NBT)
    deriving (Eq, Show)

(<+>) :: Builder -> Builder -> Builder
a <+> b = a <> B.singleton ' ' <> b

(<?>) :: (Render b) => Builder -> Maybe b -> Builder
a <?> Nothing  = a
a <?> (Just b) = a <+> (render b)

instance Render NBT where
  render nbt = error "Render NBT not implemented."

instance Render Command where
  render command =
    case command of
     (Give target item Nothing Nothing Nothing) ->
       "give" <+> render target <+> render item
     (Give target item mAmount mData mDataTag) ->
       "give" <+> render target <+> render item <+> (maybe "1" render mAmount) <+> (maybe "0" render mData) <?> mDataTag

