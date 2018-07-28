{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Data.Monoid
import Data.List
import Test.Tasty
import Test.Tasty.HUnit (testCase,Assertion,assertEqual,assertBool,assertFailure)

import Data.IORef
import Control.Monad
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
        testCase "bracketed" testBracket
    ,   testCase "over" testOver
    ,   testCase "over_" testOver_
    ,   testCase "exception" testException
    ,   testCase "for" testFor
    ,   testCase "forException" testForException
    ,   testCase "forTake" testForTake
    ]

testBracket :: Assertion
testBracket = 
    do ref <- newIORef ""
       let b = R.bracketed (modifyIORef' ref ('x':)) 
                         (\_ -> modifyIORef' ref ('y':))
                         (\_ -> S.each "abcd") 
       () :> () <- R.with b (\stream ->
           S.foldM (\() c -> modifyIORef' ref (c:)) (pure ()) pure stream)
       res <- reverse <$> readIORef ref
       assertEqual "stream results" "xabcdy" res


testOver :: Assertion
testOver = 
    do ref <- newIORef ""
       let b = R.bracketed (modifyIORef' ref ('x':)) 
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
       let b = R.bracketed (modifyIORef' ref ('x':)) 
                         (\_ -> modifyIORef' ref ('y':))
                         (\_ -> S.each "abcd") 
           b' = R.over_ (S.take 2) b
           b'' = R.over (\stream -> S.yield 'u' *> stream <* S.yield 'v') b'
           h = R.bracketed (modifyIORef' ref ('h':)) 
                         (\_ -> modifyIORef' ref ('l':))
                         (\_ -> S.each "ijk") 
       () :> () <- R.with (b'' *> h) (\stream ->
           S.foldM (\() c -> modifyIORef' ref (c:)) (pure ()) pure stream)
       res <- reverse <$> readIORef ref
       assertEqual "stream results" "uxabyvhijkl" res

testException :: Assertion
testException = 
    do ref <- newIORef ""
       let b = R.bracketed (modifyIORef' ref ('x':)) 
                         (\_ -> modifyIORef' ref ('y':))
                         (\_ -> S.yield 'a' *> S.yield 'b' *> liftIO (fail "oops"))
       _ :: Either IOException (Of () ()) <- try (R.with b (\stream ->
           S.foldM (\() c -> modifyIORef' ref (c:)) (pure ()) pure stream))
       res <- reverse <$> readIORef ref
       assertEqual "stream results" "xaby" res

testFor :: Assertion
testFor = 
    do ref <- newIORef ""
       let b = R.bracketed (modifyIORef' ref ('x':)) 
                         (\_ -> modifyIORef' ref ('y':))
                         (\_ -> S.each "ab") 
           f _ = R.bracketed (modifyIORef' ref ('u':)) 
                         (\_ -> modifyIORef' ref ('v':))
                         (\_ -> S.each "ij") 
       () :> () <- R.with (R.for b f) (\stream ->
           S.foldM (\() c -> modifyIORef' ref (c:)) (pure ()) pure stream)
       res <- reverse <$> readIORef ref
       assertEqual "stream results" "xuijvuijvy" res

testForException :: Assertion
testForException = 
    do ref <- newIORef ""
       let b = R.bracketed (modifyIORef' ref ('x':)) 
                         (\_ -> modifyIORef' ref ('y':))
                         (\_ -> S.each "ab") 
           f _ = R.bracketed (modifyIORef' ref ('u':)) 
                         (\_ -> modifyIORef' ref ('v':))
                         (\_ -> S.yield 'i' *> liftIO (fail "oops")) 
       _ :: Either IOException (Of () ()) <- try (R.with (R.for b f) (\stream ->
           S.foldM (\() c -> modifyIORef' ref (c:)) (pure ()) pure stream))
       res <- reverse <$> readIORef ref
       assertEqual "stream results" "xuivy" res

testForTake :: Assertion
testForTake = 
    do ref <- newIORef ""
       let b = R.bracketed (modifyIORef' ref ('x':)) 
                         (\_ -> modifyIORef' ref ('y':))
                         (\_ -> S.each "ab") 
           f _ = R.bracketed (modifyIORef' ref ('u':)) 
                         (\_ -> modifyIORef' ref ('v':))
                         (\_ -> S.each "ij") 
       () :> () <- R.with (R.over_ (S.take 3) (forever (R.for b f))) (\stream ->
           S.foldM (\() c -> modifyIORef' ref (c:)) (pure ()) pure stream)
       res <- reverse <$> readIORef ref
       assertEqual "stream results" "xuijvuivy" res


