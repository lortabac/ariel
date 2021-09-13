module Main where

import Criterion.Main
import Examples

main :: IO ()
main =
  defaultMain
    [ bench "eval sum" $ nfIO (exampleSum 1000000)
    ]
