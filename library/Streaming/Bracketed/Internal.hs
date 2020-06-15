{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}
module Streaming.Bracketed.Internal where

import           Data.Foldable
import           Data.Bifunctor
import           Data.IORef
import           Control.Exception

import           Streaming
import qualified Streaming.Prelude             as S

import           System.IO

{- $setup

>>> import           Data.Foldable
>>> import           Control.Monad
>>> import           System.IO
>>> import           System.FilePath
>>> import           System.Directory
>>> import           Streaming
>>> import qualified Streaming.Prelude             as S
>>> import qualified Streaming.Bracketed           as R

-}

-- | A resource management decorator for the `Stream` type.
--   
--   @a@ is the type of yielded elements, @r@ the type of the final result.
--
--   It is not parameterized by a base monad because the underlying
--   `Stream`s are always over `IO`.
newtype Bracketed a r = 
    Bracketed { runBracketed :: IORef Finstack -> Stream (Of a) IO r } 
    deriving Functor

-- | `first` maps over the yielded elements.
instance Bifunctor Bracketed where
    first f (Bracketed b) = Bracketed (S.map f . b)  
    second = fmap 

-- | `*>` performs sequential composition.
instance Applicative (Bracketed a) where
    pure = Bracketed . const . pure
    Bracketed b <*> Bracketed b' = Bracketed (\finref ->
        b finref <*> b' finref)

instance Monad (Bracketed a) where
    return = pure
    Bracketed b >>= f = Bracketed (\finref ->
        do r <- b finref
           let Bracketed b' = f r 
           b' finref)

instance MonadIO (Bracketed a) where
    liftIO action = Bracketed (\_ -> liftIO action)

-- | A stack of finalizers, accompanied by its length.
--
--   Finalizers at the head of the list correspond to deeper levels of nesting.
data Finstack = Finstack !Int [IO ()]  

{-| Lift a `Stream` that doesn't perform allocation to a `Bracketed`.
 
>>> R.with (R.clear (S.yield True)) S.toList
[True] :> ()
 
-}
clear :: Stream (Of x) IO r -> Bracketed x r
clear stream = Bracketed (const stream)

{-| Lift a `Stream` that performs resource allocation to a `Bracketed`.
 
    The first argument allocates the resource, the second is a function
    that deallocates it.
 
>>> R.with (R.bracketed (putStrLn "alloc") (\() -> putStrLn "dealloc") (\() -> S.yield True)) S.toList
alloc
dealloc
[True] :> ()

 -}
bracketed :: IO a -> (a -> IO ()) -> (a -> Stream (Of x) IO r) -> Bracketed x r
bracketed allocate finalize stream = Bracketed (\finref ->
    let open = do 
            a <- allocate
            Finstack size0 fins <- readIORef finref
            writeIORef finref (Finstack (succ size0) (finalize a : fins))
            pure (size0,a)
    in do (size0,a) <- liftIO (mask (\_ -> open))
          r <- stream a
          liftIO (mask (\_ -> reset size0 finref))
          pure r)

{-| Consume a `Bracketed` stream, exhausting it.
    
>>> R.with (pure True) S.toList
[] :> True
     
-}
with :: Bracketed a r -> (forall x. Stream (Of a) IO x -> IO (Of b x)) -> IO (Of b r)
with (Bracketed b) f = 
    Control.Exception.bracket (newIORef (Finstack 0 []))
                              (reset 0) 
                              (f . b)

{-| Consume a `Bracketed` stream, possibly wihout exhausting it.
   
    Finalizers lying in unconsumed parts of the stream will not be executed
    until the callback returns, so better not tarry too long if you want
    prompt finalization.

>>> R.with_ (R.clear (S.each "abcd" *> pure True)) (S.toList . S.take 2)
"ab" :> ()

-}
with_ :: Bracketed a r -> (Stream (Of a) IO r -> IO b) -> IO b
with_ (Bracketed b) f =
    Control.Exception.bracket (newIORef (Finstack 0 []))
                              (reset 0) 
                              (f . b)

{-| Apply to the underlying stream a transformation that preserves the return value, like 'S.map'.

>>> R.with (S.map succ `R.over` R.clear (S.each "abcd")) S.toList
"bcde" :> ()

-}
over :: (forall x. Stream (Of a) IO x -> Stream (Of b) IO x) -> Bracketed a r -> Bracketed b r 
over transform (Bracketed b) = Bracketed (transform . b)


{-| Like 'over', but for transformations which return some final state or summary value besides the original return value.

-}
over' :: (forall x. Stream (Of a) IO x -> Stream (Of b) IO (Of s x)) -> Bracketed a r -> Bracketed b (Of s r)
over' transform (Bracketed b) = Bracketed (transform . b)


{-| Apply to the underlying stream a transformation that might not preserve
    the return value, like 'S.take'.

>>> R.with (S.take 2 `R.over_` R.clear (S.each "abdc")) S.toList
"ab" :> ()
     
-}
over_ :: (Stream (Of a) IO r -> Stream (Of b) IO r') -> Bracketed a r -> Bracketed b r'
over_ transform (Bracketed b) = Bracketed (\finref ->
    let level = do
            Finstack size _ <- readIORef finref
            pure size
    in do size0 <- liftIO level
          r <- transform (b finref)
          liftIO (mask (\_ -> reset size0 finref))
          pure r)

-- | Replaces each element of a stream with an associated stream. 
--
--   Can be useful for traversing hierachical structures.
for :: Bracketed a r -> (a -> Bracketed b x) -> Bracketed b r
for (Bracketed b) f = 
    Bracketed (\fins -> S.for (b fins) (flip (runBracketed . f) fins))
    
-- | Executes all finalizers that lie above a certain level.
reset :: Int -> IORef Finstack -> IO ()
reset size0 finref =
    do Finstack size fins <- readIORef finref
       let (pending,fins') = splitAt (size - size0) fins 
       writeIORef finref (Finstack size0 fins')
       foldr finally (pure ()) pending 

-- | A bracketed stream of all the lines in a text file.
--
--   This is adequate for simple use cases. For more advanced ones where
--   efficiency and memory usage are important, it's better to use a packed
--   text representation like the one provided by the @text@ package.
linesFromFile :: TextEncoding -> NewlineMode -> FilePath -> Bracketed String ()
linesFromFile encoding newlineMode path = bracketed
  (openFile path ReadMode)
  hClose
  (\h -> do
    liftIO (hSetEncoding h encoding)
    liftIO (hSetNewlineMode h newlineMode)
    S.untilRight
      (do
        eof <- hIsEOF h
        if eof then Right <$> pure () else Left <$> hGetLine h
      )
  )

-- | Given a list of text files and line ranges, create a stream of lines
--   belonging to the concatenated ranges.
concatRanges
  :: TextEncoding
  -> NewlineMode
  -> [(FilePath, Int, Int)]
  -> Bracketed String ()
concatRanges encoding newlineMode ranges =
  let streamRange (path, start, end) =
        over_ (S.take (end - start)) . over (S.drop start) $ linesFromFile
          encoding
          newlineMode
          path
  in  traverse_ streamRange ranges

