-- | A resource management decorator for `Stream`s.
--
--   Resource-allocating 'Stream's are lifted to values of type `Bracketed` and
--   can then be combined as such, using an API that is more restricted than
--   that of the original 'Stream's and ensures prompt deallocation of
--   resources.
--
--   Values of type `Bracketed` can later be run by supplying a
--   'Stream'-consumer continuation to the 'with' function.
module Streaming.Bracketed (
      -- * Bracketed
      Bracketed
      -- * Lifting streams
    , clear
    , bracketed
      -- * Consuming bracketed streams with continuations
    , with
    , with_
      -- * Transforming bracketed streams
    , over
    , over_
    , for 
      -- * Reading text files
    , linesFromFile
    , concatRanges
    ) where

import           Streaming
import           Streaming.Bracketed.Internal 

