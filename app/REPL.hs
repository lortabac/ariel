{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module REPL where

import Ariel.Common.Types
import Ariel.Prelude
import Ariel.Syntax.Eval
import Ariel.TC.Context
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
  runInputT settings (loop emptyTCCtx)
  where
    loop :: TCCtx -> InputT IO ()
    loop ctx = do
      mInput <- getInputLine "> "
      case mInput of
        Nothing -> pure ()
        Just "" -> loop ctx
        Just ":quit" -> pure ()
        Just ":q" -> pure ()
        Just ":exit" -> pure ()
        Just (':' : cmd) -> case break (== ' ') cmd of
          ("t", expr) -> case typecheckArielStr ctx (LBS.pack expr) of
                Right ty -> liftIO (LBS.putStrLn ty) >> loop ctx
                Left errs -> do
                  traverse_ (liftIO . TIO.putStrLn) errs
                  loop ctx
          _ -> liftIO (putStrLn "Invalid command") >> loop ctx
        Just expr -> do
              res <- liftIO $ runArielStr ctx (LBS.pack expr)
              case res of
                Right (ExprOutcome r) -> liftIO (LBS.putStrLn r) >> loop ctx
                Right (DeclOutcome qname@(QName _ name) ty e) ->
                  let newNS = Map.insertWith (<>) "user" [name] (names ctx)
                      newDefs = Map.insert qname e (globals ctx)
                      newTyCtx = extendGlobalCtx qname ty (tyCtx ctx)
                      newCtx = ctx
                        { names = newNS
                        , tyCtx = newTyCtx
                        , globals = newDefs
                        }
                   in loop newCtx
                Left errs -> do
                  traverse_ (liftIO . TIO.putStrLn) errs
                  loop ctx

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
