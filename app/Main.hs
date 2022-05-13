module Main where

import qualified Data.ByteString.Lazy.Char8 as LBS
import Examples

main :: IO ()
main = LBS.putStrLn =<< exampleSumr
