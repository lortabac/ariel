{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Ariel.Syntax.Types where

import Control.DeepSeq
import Data.String (IsString)
import GHC.Generics

-- | De Brujin index
newtype VarIx = VarIx Int deriving (Eq, Num, Generic)

instance NFData VarIx

instance Show VarIx where
  show (VarIx i) = show i

-- | Variant index
newtype ConsIx = ConsIx Int deriving (Eq, Num, Show, Generic)

instance NFData ConsIx

-- | Variant tag
newtype Tag = Tag {unTag :: String} deriving (Eq, Ord, Show, IsString, Generic)

instance NFData Tag

-- | Tuple index
newtype TupleIx = TupleIx Int deriving (Eq, Num, Show, Generic)

instance NFData TupleIx

-- | Variable name
newtype Name = Name {unName :: String} deriving (Eq, Ord, IsString, Generic)

instance NFData Name

instance Show Name where
  show (Name n) = show n
