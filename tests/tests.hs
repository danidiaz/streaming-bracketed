{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
module Main where

import Data.Monoid
import Data.List
import Test.Tasty
import Test.Tasty.HUnit (testCase,Assertion,assertEqual,assertBool,assertFailure)

import Data.IORef

import Streaming
import qualified Streaming.Prelude as S
import qualified Streaming.Bracketed as R

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = 
    testGroup "All" 
    [
        testCase "bracket" bracketTest
    ]

bracketTest :: Assertion
bracketTest = 
    do ref <- newIORef ""
       let b = R.bracket (modifyIORef' ref ('x':)) 
                         (\_ -> modifyIORef' ref ('y':))
                         (\_ -> S.each "abcd") 
       () :> () <- R.with b (\stream ->
           S.foldM (\() c -> modifyIORef' ref (c:)) (pure ()) pure stream)
       res <- reverse <$> readIORef ref
       assertEqual "stream results" "xabcdy" res


