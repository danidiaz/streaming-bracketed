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
from = undefined

bracket :: IO a -> (a -> IO ()) -> (a -> Stream (Of x) IO r) -> Bracketed x r
bracket = undefined

with :: Bracketed a r -> (forall x. Stream (Of a) IO x -> IO (Of b x)) -> IO (Of b r)
with _ _ = undefined

with_ :: Bracketed a r -> (forall x. Stream (Of a) IO x -> IO b) -> IO b
with_ _ _ = undefined

over :: (Stream (Of a) IO r -> Stream (Of b) IO r) -> Bracketed a r -> Bracketed b r 
over = undefined

over_ :: (Stream (Of a) IO r -> Stream (Of b) IO r') -> Bracketed a r -> Bracketed b r'
over_ = undefined

for :: Bracketed a r -> (a -> Bracketed b x) -> Bracketed b r
for = undefined
