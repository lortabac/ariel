module Ariel.Prelude
  ( tshow,
    lbsToText,
    module Control.Monad.Reader,
    module Control.Monad.State,
    module Data.Data,
    module Data.Foldable,
    module Data.Map,
    module Data.Proxy,
    module Data.Set,
    module Data.Text,
    module Data.Traversable,
  )
where

import Control.Monad.Reader
import Control.Monad.State
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Data (Data)
import Data.Foldable (for_, toList, traverse_)
import Data.Map (Map)
import Data.Proxy (Proxy (..))
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.Traversable (for)

tshow :: Show a => a -> Text
tshow = T.pack . show

lbsToText :: LBS.ByteString -> Text
lbsToText = LT.toStrict . decodeUtf8
