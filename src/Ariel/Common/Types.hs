{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Ariel.Common.Types where

import Data.String (IsString)
import Data.Text (Text)

data QName = QName Text Text deriving (Eq, Ord, Show)

newtype Tag = Tag Text deriving (Eq, Ord, Show, IsString)

newtype Label = Label Text deriving (Eq, Ord, Show, IsString)
