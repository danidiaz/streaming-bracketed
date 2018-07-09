module Streaming.Bracketed.Internal where

import           Streaming
import qualified Streaming.Prelude as S

import           Data.IORef
import           Control.Monad.Catch as C


newtype Bracketed a r = 
    Bracketed { runBracketed :: IORef [IO ()] -> Stream (Of a) IO r } 
    deriving Functor
