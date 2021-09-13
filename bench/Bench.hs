module Main where

import Examples
import Criterion.Main

main :: IO ()
main = defaultMain
     [ bench "benchmark sum" $ nfIO (example_Sum 1000000)
     ]
