{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module REPL where

import Ariel.Common.Types
import qualified Ariel.Core.Types as Core
import Ariel.Prelude
import Ariel.Syntax.Eval
import Control.Monad.Catch
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Map as Map
import qualified Data.Text.IO as TIO
import NeatInterpolation
import System.Console.Haskeline
import System.Directory (XdgDirectory (..), createDirectoryIfMissing, getXdgDirectory)
import System.FilePath ((</>))

repl :: IO ()
repl = do
  TIO.putStrLn help
  putStrLn ""
  historyPath <- getHistoryFilePath
  let settings =
        defaultSettings
          { historyFile = Just historyPath,
            autoAddHistory = True
          }
  runInputT settings (loop mempty mempty)
  where
    loop :: Map Text (Set Name) -> Core.Defs -> InputT IO ()
    loop ns defs = do
      mInput <- getInputLine "> "
      case mInput of
        Nothing -> pure ()
        Just "" -> loop ns defs
        Just ":quit" -> pure ()
        Just ":q" -> pure ()
        Just ":exit" -> pure ()
        Just (':' : cmd) -> case break (== ' ') cmd of
          ("t", expr) -> case typecheckArielStr ns defs (LBS.pack expr) of
                Right ty -> liftIO (LBS.putStrLn ty) >> loop ns defs
                Left errs -> do
                  traverse_ (liftIO . TIO.putStrLn) errs
                  loop ns defs
          _ -> liftIO (putStrLn "Invalid command") >> loop ns defs
        Just expr -> do
              res <- liftIO $ runArielStr ns defs (LBS.pack expr)
              case res of
                Right (ExprOutcome r) -> liftIO (LBS.putStrLn r) >> loop ns defs
                Right (DeclOutcome (QName _ name) _ newDefs) ->
                  let newNS = Map.insert "user" [name] ns
                   in loop newNS newDefs
                Left errs -> do
                  traverse_ (liftIO . TIO.putStrLn) errs
                  loop ns defs

getHistoryFilePath :: IO FilePath
getHistoryFilePath = do
  dir <- getXdgDirectory XdgData "ariel"
  createDirectoryIfMissing True dir
  pure (dir </> "history")

help :: Text
help =
  [trimming|
This is the interactive interpreter for Ariel version 0.0.0.0

Commands available from the prompt:

  <expr>                 evaluate an expression
  :t <expr> ...          find the type of an expression
  :quit                  exit the interpreter

|]
