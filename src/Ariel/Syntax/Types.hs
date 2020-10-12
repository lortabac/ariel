{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Ariel.Syntax.Types where

import Data.String (IsString)

-- | Variant tag
newtype Tag = Tag {unTag :: String} deriving (Eq, Ord, Show, IsString)

-- | Field label
newtype Label = Label {unLabel :: String} deriving (Eq, Ord, Show, IsString)

-- | Variable name
newtype Name = Name {unName :: String} deriving (Eq, Ord, Show, IsString)
