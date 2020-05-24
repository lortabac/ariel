{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Ariel.Syntax.Types where

import Data.String (IsString)

-- | De Brujin index
newtype VarIx = VarIx Int deriving (Eq, Num, Show)

-- | Variant index
newtype ConsIx = ConsIx Int deriving (Eq, Num, Show)

-- | Variant tag
newtype Tag = Tag {unTag :: String} deriving (Eq, Ord, Show, IsString)

-- | Tuple index
newtype TupleIx = TupleIx Int deriving (Eq, Num, Show)

-- | Variable name
newtype Name = Name {unName :: String} deriving (Eq, Ord, Show, IsString)
