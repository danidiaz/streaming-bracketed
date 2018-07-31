module Main where

import Test.DocTest

main :: IO ()
main = doctest
  ["-ilibrary","library/Streaming/Bracketed.hs","library/Streaming/Bracketed/Internal.hs"]
