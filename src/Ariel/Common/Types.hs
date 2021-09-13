{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Ariel.Common.Types where

import Control.DeepSeq
import Data.String (IsString)
import Data.Text (Text)
import GHC.Generics

data QName = QName Text Text deriving (Eq, Ord, Show, Generic)

instance NFData QName

newtype Tag = Tag Text deriving (Eq, Ord, Show, IsString, Generic)

instance NFData Tag

newtype Label = Label Text deriving (Eq, Ord, Show, IsString, Generic)

instance NFData Label
