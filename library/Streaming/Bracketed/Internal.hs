{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}
module Streaming.Bracketed.Internal where

import           Data.IORef
import           Control.Exception

import           Streaming
import qualified Streaming.Prelude as S

newtype Bracketed a r = 
    Bracketed { runBracketed :: IORef Finstack -> Stream (Of a) IO r } 
    deriving Functor

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

data Finstack = Finstack !Int [IO ()]  

from :: Stream (Of x) IO r -> Bracketed x r
from stream = Bracketed (const stream)

bracket :: IO a -> (a -> IO ()) -> (a -> Stream (Of x) IO r) -> Bracketed x r
bracket allocate finalize stream = Bracketed (\finref ->
    let open = do 
            a <- allocate
            Finstack size0 fins <- readIORef finref
            writeIORef finref (Finstack (succ size0) (finalize a : fins))
            pure (size0,a)
    in do (size0,a) <- liftIO (mask (\_ -> open))
          r <- stream a
          liftIO (mask (\_ -> reset size0 finref))
          pure r)

with :: Bracketed a r -> (forall x. Stream (Of a) IO x -> IO (Of b x)) -> IO (Of b r)
with (Bracketed b) f = 
    Control.Exception.bracket (newIORef (Finstack 0 []))
                              (reset 0) 
                              (f . b)

with_ :: Bracketed a r -> (forall x. Stream (Of a) IO x -> IO b) -> IO b
with_ (Bracketed b) f =
    Control.Exception.bracket (newIORef (Finstack 0 []))
                              (reset 0) 
                              (f . b)

over :: (Stream (Of a) IO r -> Stream (Of b) IO r) -> Bracketed a r -> Bracketed b r 
over transform (Bracketed b) = Bracketed (transform . b)

over_ :: (Stream (Of a) IO r -> Stream (Of b) IO r') -> Bracketed a r -> Bracketed b r'
over_ transform (Bracketed b) = Bracketed (\finref ->
    let level = do
            Finstack size _ <- readIORef finref
            pure size
    in do size0 <- liftIO level
          r <- transform (b finref)
          liftIO (mask (\_ -> reset size0 finref))
          pure r)

for :: Bracketed a r -> (a -> Bracketed b x) -> Bracketed b r
for (Bracketed b) f = 
    Bracketed (\fins -> S.for (b fins) (flip (runBracketed . f) fins))
    
reset :: Int -> IORef Finstack -> IO ()
reset size0 finref =
    do Finstack size fins <- readIORef finref
       let (pending,fins') = splitAt (size-size0) fins 
       writeIORef finref (Finstack size0 fins')
       foldr finally (pure ()) fins 

