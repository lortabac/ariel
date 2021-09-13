module Main where

import Examples

import Criterion.Main


main :: IO ()
main = defaultMain
     [ bench "eval sum" $ nfIO (exampleSum 1000000)
     ]
