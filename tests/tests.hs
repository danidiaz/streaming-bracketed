{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
module Main where

import           Data.Monoid
import           Data.Foldable
import           Data.List
import           Data.List.NonEmpty             ( NonEmpty(..) )
import qualified Data.List.NonEmpty
import           Data.Tree
import           Test.Tasty
import           Test.Tasty.HUnit               ( testCase
                                                , Assertion
                                                , assertEqual
                                                , assertBool
                                                , assertFailure
                                                )

import           Data.IORef
import           Control.Monad
import           Control.Exception
    
import           System.IO
import           System.FilePath
import           System.Directory

import           Streaming
import qualified Streaming.Prelude             as S
import qualified Streaming.Bracketed           as R

import qualified Data.Streaming.Filesystem     as FS -- streaming-commons

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup
  "All"
  [ testCase "bracketed"    testBracket
  , testCase "over"         testOver
  , testCase "over_"        testOver_
  , testCase "exception"    testException
  , testCase "for"          testFor
  , testCase "forException" testForException
  , testCase "forTake"      testForTake
  , testGroup "dir traversals" [testCase "testDirTraversal" testDirTraversal]
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

directoryTree :: Tree (FilePath, [FilePath])
directoryTree = Node
  ("a", ["file1", "file2"])
  [ Node
    ("aa", ["file3", "file4"])
    [ Node ("aaa", ["file5"])
           [Node ("aaaa", ["file6"]) [], Node ("aaab", ["file7", "file8"]) []]
    ]
  , Node
    ("ab", ["file9", "file10"])
    [ Node
      ("aba", ["file11"])
      [ Node ("abaa", ["file12"])           []
      , Node ("abab", ["file13", "file14"]) []
      , Node ("abac", ["file15"])           []
      , Node ("abad", ["file16", "file17"]) []
      ]
    , Node ("abb", ["file18"]) []
    ]
  , Node ("ac", []) []
  , Node ("ad", []) [Node ("ada", []) []]
  ]

createHierarchy :: Tree (FilePath, [FilePath]) -> FilePath -> IO ()
createHierarchy = 
        let alg (dir,filenames) downwards ((</> dir) -> base) = do
                createDirectory base
                let filepaths = (base </>) <$> filenames
                for_ filepaths (\path -> withFile path WriteMode (\_ -> pure ())) 
                for_ downwards ($ base)
         in foldTree alg 

testDirTraversal :: Assertion
testDirTraversal =  do
    -- http://hackage.haskell.org/package/directory-1.3.3.0/docs/System-Directory.html
    -- http://hackage.haskell.org/package/filepath-1.4.2.1/docs/System-FilePath-Posix.html
    let baseDir = "__3hgal34_streaming_bracketed_testTreeTraversal_"
    testDir <- fmap (</> baseDir) getTemporaryDirectory 
    do testDirExists <- doesPathExist testDir 
       when testDirExists (removePathForcibly testDir)
    createHierarchy  directoryTree testDir
                    

     
