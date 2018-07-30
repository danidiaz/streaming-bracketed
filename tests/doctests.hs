module Main where

import Test.DocTest

main :: IO ()
main = doctest
  ["library/Streaming/Bracketed.hs"]
