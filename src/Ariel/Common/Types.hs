{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Ariel.Common.Types where

import Data.String (IsString)
import Data.Void (Void)

-- | Language phase
data Phase = Full | Desugared | Core

type family Until (until :: Phase) (ph :: Phase) a where
  Until 'Core _ a = a
  Until 'Desugared 'Core _ = ()
  Until 'Desugared _ a = a
  Until 'Full 'Full a = a
  Until 'Full _ _ = ()

type family UntilS (until :: Phase) (ph :: Phase) a where
  UntilS 'Core _ a = a
  UntilS 'Desugared 'Core _ = Void
  UntilS 'Desugared _ a = a
  UntilS 'Full 'Full a = a
  UntilS 'Full _ _ = Void

type family CoreOnly (ph :: Phase) a where
  CoreOnly 'Core a = a
  CoreOnly _ _ = ()

type family CoreOnlyS (ph :: Phase) a where
  CoreOnlyS 'Core a = a
  CoreOnlyS _ _ = Void

-- | Variant index
newtype ConsIx = ConsIx Int deriving (Eq, Num, Show)

-- | Variant tag
newtype Tag = Tag {unTag :: String} deriving (Eq, Ord, Show, IsString)

-- | Tuple index
newtype TupleIx = TupleIx Int deriving (Eq, Num, Show)

-- | Variable name
newtype Name = Name {unName :: String} deriving (Eq, Ord, Show, IsString)
