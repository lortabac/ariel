{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}

module Ariel.Hoas.IO where

import Ariel.Common.Env
import Ariel.Common.Types
import Ariel.Hoas.Expr
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8, encodeUtf8)

-- | Make an IO primitive
ioPrim :: Env ExprH -> Name -> ExprH
ioPrim env name = case name of
  "getLine#" -> primGetLine
  "putStr#" -> primPutStr env
  "putStrLn#" -> primPutStrLn env
  "readFile#" -> primReadFile env
  "writeFile#" -> primWriteFile env
  _ -> error "Invalid IO primitive"

primGetLine :: ExprH
primGetLine = IOH (fromByteString <$> BS.getLine)

primPutStr :: Env ExprH -> ExprH
primPutStr env = LamH $ \k s ->
  k env $ IOH (hvoid (BS.putStr (toByteString s)))

primPutStrLn :: Env ExprH -> ExprH
primPutStrLn env = LamH $ \k s ->
  k env $ IOH (hvoid (BS.putStrLn (toByteString s)))

primReadFile :: Env ExprH -> ExprH
primReadFile env = LamH $ \k path ->
  k env $ IOH (StringH . decodeUtf8 <$> BS.readFile (toString path))

primWriteFile :: Env ExprH -> ExprH
primWriteFile env = LamH $ \k path ->
  k env $ LamH $ \l content ->
    l env $ IOH (hvoid (BS.writeFile (toString path) (toByteString content)))

fromStringH :: ExprH -> Text
fromStringH (StringH s) = s
fromStringH _ = error "Not a string"

hunit :: ExprH
hunit = TupleH mempty

hvoid :: Monad m => m a -> m ExprH
hvoid f = f >> pure hunit

toString :: ExprH -> String
toString = Text.unpack . fromStringH

toByteString :: ExprH -> ByteString
toByteString = encodeUtf8 . fromStringH

fromByteString :: ByteString -> ExprH
fromByteString = StringH . decodeUtf8
