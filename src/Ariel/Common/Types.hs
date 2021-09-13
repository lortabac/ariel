{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}

module Ariel.Common.Types where

import Data.String (IsString)
import Data.Text (Text)
import Control.DeepSeq
import GHC.Generics

data QName = QName Text Text deriving (Eq, Ord, Show, Generic)

instance NFData QName

newtype Tag = Tag Text deriving (Eq, Ord, Show, IsString, Generic)

instance NFData Tag

newtype Label = Label Text deriving (Eq, Ord, Show, IsString, Generic)

instance NFData Label
