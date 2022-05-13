{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}

module Ariel.Common.Types where

import Control.DeepSeq
import Data.Coerce
import Data.Data
import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics
import Language.SexpGrammar
import Language.SexpGrammar.Generic

newtype Name = Name {unName :: Text}
  deriving (Eq, Ord, Show, IsString, Generic, Data)

instance NFData Name

showName :: Name -> String
showName (Name n) = T.unpack n

instance SexpIso Name where
  sexpIso = with (symbol >>>)

data QName = QName Text Name deriving (Eq, Ord, Show, Generic, Data)

instance NFData QName

newtype Tag = Tag Text deriving (Eq, Ord, Show, IsString, Generic)

instance SexpIso Tag where
  sexpIso = with $ \tag -> quoted symbol >>> tag

instance NFData Tag

newtype Label = Label Text deriving (Eq, Ord, Show, IsString, Generic)

instance SexpIso Label where
  sexpIso = with $ \label -> keyword >>> label

instance NFData Label

-- | Type variable
newtype TyVar = TyVar {unTyVar :: Name}
  deriving stock (Eq, Ord, Show, Generic, Data)
  deriving newtype (IsString)

instance SexpIso TyVar where
  sexpIso = with (sexpIso >>>)

tyVarSupply :: [TyVar]
tyVarSupply = go (TyVar "a")
  where
    go tv = tv : go (getNextTyVar tv)
    getNextTyVar = coerce . T.pack . nextTyVar . T.unpack . coerce

nextTyVar :: String -> String
nextTyVar "z" = "z1"
nextTyVar ('z' : i) = 'z' : show (read i + 1 :: Int)
nextTyVar [c] = [succ c]
nextTyVar tv = error ("Invalid nextTyVar: " <> tv)
