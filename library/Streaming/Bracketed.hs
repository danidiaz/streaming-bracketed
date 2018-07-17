-- | A resource management decorator for `Stream`s.
module Streaming.Bracketed (
      Bracketed
    , from
    , bracket
    , with
    , with_
    , over
    , over_
    , for 
    ) where

import Streaming.Bracketed.Internal 

