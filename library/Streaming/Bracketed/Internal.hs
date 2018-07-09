{-# LANGUAGE DeriveFunctor #-}
module Streaming.Bracketed.Internal where

import           Streaming
import qualified Streaming.Prelude as S

import           Data.IORef
import           Control.Monad.Catch as C


data Finstack = Finstack !Int ![IO ()]  

newtype Bracketed a r = 
    Bracketed { runBracketed :: IORef Finstack -> Stream (Of a) IO r } 
    deriving Functor
