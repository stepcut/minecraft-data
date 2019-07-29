module Main where

{-
https://minecraft.gamepedia.com/Java_Edition_data_values#Blocks
-}

import Data.Char (toUpper)
import Data.List (intercalate)
import System.IO

toCamel :: String -> String
toCamel [] = []
toCamel (c:cs) = toUpper c : toCamel' cs
  where
    toCamel' ('_':cs) = toCamel cs
    toCamel' (c:cs) = c : toCamel' cs
    toCamel' [] = []

heading :: [ String ]
heading =
  [ "{-# language DeriveDataTypeable #-}"
  , "{-# language DeriveGeneric #-}"
  , "{-# language OverloadedStrings #-}"
  , "module Minecraft.Block where"
  , ""
  , "import Data.Bimap (Bimap, fromList)"
  , "import Data.Data"
  , "import Data.Text (Text)"
  , "import qualified Data.Text as Text"
  , "import GHC.Generics"
  ]

namespacedId :: [String]
namespacedId =
  [ "data NamespacedId = NamespacedId { namespace :: Text, name :: Text }"
  , "  deriving (Eq, Ord, Read, Show, Data, Generic)"
  , []
  , "formatNamespacedId :: NamespacedId -> Text"
  , "formatNamespacedId (NamespacedId ns n) = ns <> Text.singleton ':' <> n"
  ]

genBlock :: [String] -> [String]
genBlock (b:bs) =
  "data Block" : ("  = " ++ (toCamel b)) : (map (\b -> ("  | "  ++ (toCamel b))) bs) ++
  ["  deriving (Eq, Ord, Read, Show, Enum, Data, Generic)"]

genBlockNames :: [String] -> [String]
genBlockNames (b:blks) =
  [ "blockNames :: Bimap Block NamespacedId"
  , "blockNames = fromList ["
  , "    (" ++ (toCamel b) ++ ", NamespacedId \"minecraft\" \"" ++ b ++ "\")"
  ] ++ (map mkEntry blks) ++ [ "  ]" ]
  where
    mkEntry blk =
      "  , (" ++ toCamel blk ++ ", NamespacedId \"minecraft\" \"" ++ blk ++ "\")"

main =
  do blks <- lines <$> readFile "block_namespaced_ids.txt"
     putStrLn $ unlines $ intercalate [[]]
        [ heading
        , namespacedId
        , genBlock blks
        , genBlockNames blks
        ]
