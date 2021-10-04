{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Ariel.Common.Types where

import Control.DeepSeq
import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics
import Language.SexpGrammar
import Language.SexpGrammar.Generic

newtype Name = Name {unName :: Text}
  deriving (Eq, Ord, Show, IsString, Generic)

instance NFData Name

instance SexpIso Name where
  sexpIso = with (symbol >>>)

data QName = QName Text Text deriving (Eq, Ord, Show, Generic)

instance SexpIso QName where
  sexpIso = with $
    \qname -> list (el (sym "q") >>> el symbol >>> el symbol) >>> qname

instance NFData QName

newtype Tag = Tag Text deriving (Eq, Ord, Show, IsString, Generic)

instance SexpIso Tag where
  sexpIso = with $ \tag -> quoted symbol >>> tag

instance NFData Tag

newtype Label = Label Text deriving (Eq, Ord, Show, IsString, Generic)

instance SexpIso Label where
  sexpIso = with $ \label -> keyword >>> label

instance NFData Label

data Ty
  = TInt
  | TString
  | TArr Ty Ty
  | TVar TyVar
  | Forall [TyVar] Ty
  deriving (Eq, Show, Generic)

instance SexpIso Ty where
  sexpIso = match $
    With (sym "Int" >>>) $
      With (sym "String" >>>) $
        With (list (el (sym "->") >>> el sexpIso >>> el sexpIso) >>>) $
          With (sexpIso >>>) $
            With (list (el (sym "forall") >>> el sexpIso >>> el sexpIso) >>>) End

-- | Type variable
newtype TyVar = TyVar {unTyVar :: Text} deriving (Eq, Ord, Show, IsString, Generic)

instance SexpIso TyVar where
  sexpIso = with (symbol >>>)

tyVarSupply :: [TyVar]
tyVarSupply = go (TyVar "a")
  where
    go tv = tv : go (getNextTyVar tv)
    getNextTyVar = TyVar . T.pack . nextTyVar . T.unpack . unTyVar

nextTyVar :: String -> String
nextTyVar "z" = "z1"
nextTyVar ('z' : i) = 'z' : show (read i + 1 :: Int)
nextTyVar [c] = [succ c]
nextTyVar tv = error ("Invalid nextTyVar: " <> tv)
