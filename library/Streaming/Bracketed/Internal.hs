{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
module Streaming.Bracketed.Internal where

import           Streaming
import qualified Streaming.Prelude as S

import           Data.IORef
import           Control.Monad.Catch as C


newtype Bracketed a r = 
    Bracketed { runBracketed :: IORef Finstack -> Stream (Of a) IO r } 
    deriving Functor

data Finstack = Finstack !Int [IO ()]  

from :: Stream (Of x) IO r -> Bracketed x r
from stream = Bracketed (const stream)

bracket :: IO a -> (a -> IO ()) -> (a -> Stream (Of x) IO r) -> Bracketed x r
bracket allocate finalize stream = Bracketed (\finref ->
    do let open = do a <- allocate
                     modifyIORef' finref (\(Finstack size fins) -> Finstack (succ size) (finalize a : fins))
                     pure a
           close = modifyIORef' finref (\(Finstack size (fin:fins)) -> Finstack (pred size) fins)
       a <- liftIO open
       r <- stream a
       liftIO (finalize a)
       liftIO close
       pure r)

with :: Bracketed a r -> (forall x. Stream (Of a) IO x -> IO (Of b x)) -> IO (Of b r)
with (Bracketed b) f = 
    do fins <- newIORef (Finstack 0 [])
       f (b fins)

with_ :: Bracketed a r -> (forall x. Stream (Of a) IO x -> IO b) -> IO b
with_ (Bracketed b) f =
    do fins <- newIORef (Finstack 0 [])
       f (b fins)

over :: (Stream (Of a) IO r -> Stream (Of b) IO r) -> Bracketed a r -> Bracketed b r 
over transform (Bracketed b) = Bracketed (transform . b)

over_ :: (Stream (Of a) IO r -> Stream (Of b) IO r') -> Bracketed a r -> Bracketed b r'
over_ = undefined

for :: Bracketed a r -> (a -> Bracketed b x) -> Bracketed b r
for (Bracketed b) f = 
    Bracketed (\fins -> S.for (b fins) (flip (runBracketed . f) fins))
    
-- TODO remember to add the masks!
-- TODO applicative and monad instances.

