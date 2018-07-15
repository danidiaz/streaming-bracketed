{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Data.Monoid
import Data.List
import Test.Tasty
import Test.Tasty.HUnit (testCase,Assertion,assertEqual,assertBool,assertFailure)

import Data.IORef
import Control.Exception

import Streaming
import qualified Streaming.Prelude as S
import qualified Streaming.Bracketed as R

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = 
    testGroup "All" 
    [
        testCase "bracket" testBracket
    ,   testCase "over" testOver
    ,   testCase "over_" testOver_
    ,   testCase "exception" testException
    ]

testBracket :: Assertion
testBracket = 
    do ref <- newIORef ""
       let b = R.bracket (modifyIORef' ref ('x':)) 
                         (\_ -> modifyIORef' ref ('y':))
                         (\_ -> S.each "abcd") 
       () :> () <- R.with b (\stream ->
           S.foldM (\() c -> modifyIORef' ref (c:)) (pure ()) pure stream)
       res <- reverse <$> readIORef ref
       assertEqual "stream results" "xabcdy" res


testOver :: Assertion
testOver = 
    do ref <- newIORef ""
       let b = R.bracket (modifyIORef' ref ('x':)) 
                         (\_ -> modifyIORef' ref ('y':))
                         (\_ -> S.each "abcd") 
           b' = R.over (\stream -> S.yield 'u' *> stream <* S.yield 'v') b
       () :> () <- R.with b' (\stream ->
           S.foldM (\() c -> modifyIORef' ref (c:)) (pure ()) pure stream)
       res <- reverse <$> readIORef ref
       assertEqual "stream results" "uxabcdyv" res

testOver_ :: Assertion
testOver_ = 
    do ref <- newIORef ""
       let b = R.bracket (modifyIORef' ref ('x':)) 
                         (\_ -> modifyIORef' ref ('y':))
                         (\_ -> S.each "abcd") 
           b' = R.over_ (S.take 2) b
           b'' = R.over (\stream -> S.yield 'u' *> stream <* S.yield 'v') b'
           h = R.bracket (modifyIORef' ref ('h':)) 
                         (\_ -> modifyIORef' ref ('l':))
                         (\_ -> S.each "ijk") 
       () :> () <- R.with (b'' *> h) (\stream ->
           S.foldM (\() c -> modifyIORef' ref (c:)) (pure ()) pure stream)
       res <- reverse <$> readIORef ref
       assertEqual "stream results" "uxabyvhijkl" res

testException :: Assertion
testException = 
    do ref <- newIORef ""
       let b = R.bracket (modifyIORef' ref ('x':)) 
                         (\_ -> modifyIORef' ref ('y':))
                         (\_ -> S.yield 'a' *> S.yield 'b' *> liftIO (fail "oops"))
       _ :: Either IOException (Of () ()) <- try (R.with b (\stream ->
           S.foldM (\() c -> modifyIORef' ref (c:)) (pure ()) pure stream))
       res <- reverse <$> readIORef ref
       assertEqual "stream results" "xaby" res


