-- | A resource management decorator for `Stream`s.
--
--   Resource-dependent `Stream`s are lifted to values of type `Bracketed` and
--   can be combined as such, using an api that is more restricted than that of
--   the original `Stream`s.
--
--   Values of type `Bracketed` can later be run by supplying a
--   `Stream`-consumer continuation to the `with` function.
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
    ) where

import Streaming.Bracketed.Internal 

