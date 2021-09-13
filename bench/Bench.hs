module Main where

import Criterion.Main
import Examples

main :: IO ()
main =
  defaultMain
    [ bench "benchmark sum" $ nfIO (example_Sum 1000000)
    ]
