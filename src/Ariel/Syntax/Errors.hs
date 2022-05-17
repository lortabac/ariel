{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Ariel.Syntax.Errors where

import Ariel.Common.Types
import Ariel.Core.Types
import Ariel.Prelude
import Ariel.Syntax.ReadBack
import Ariel.TC.Types
import Language.Sexp.Located (Position (..))
import Language.SexpGrammar (encode)
import NeatInterpolation

showTCMessage :: TCMessage -> (Position, Text)
showTCMessage (TCMessage p err) = (p, showTCError err)

showTCError :: TCError -> Text
showTCError (TypeMismatch t1 t2) =
  let s1 = encodeTy t1
      s2 = encodeTy t2
   in [trimming|
  Expected '${s1}' but got '${s2}'
  |]
showTCError (SubsumptionError t1 t2) =
  let s1 = encodeTy t1
      s2 = encodeTy t2
   in [trimming|
  '${s1}' is not as polymorphic as '${s2}'
  |]
showTCError (OutOfScopeVar name) =
  let s = encodeName name
   in [trimming|
  The identifier '${s}' is out of scope
  |]
showTCError (OutOfScopeTyVar t) =
  let s = encodeTy (TVar t)
   in [trimming|
  The type variable '${s}' is out of scope
  |]
showTCError (CyclicType t) =
  let s = encodeTy t
   in [trimming|
  The type '${s}' is cyclic
  |]
showTCError (InvalidPrim t) =
  [trimming|
  Invalid prim '${t}' or invalid number of arguments
  |]
showTCError (InvalidIOPrim t) =
  [trimming|
  Invalid io-prim '${t}' or invalid number of arguments
  |]

encodeTy :: Ty -> Text
encodeTy e = case encode (readBackTy e) of
  Right t -> lbsToText t
  Left err -> error err

encodeName :: Name -> Text
encodeName name = case encode name of
  Right e -> lbsToText e
  Left err -> error err
