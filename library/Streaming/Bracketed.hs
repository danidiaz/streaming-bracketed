{-| A resource management decorator for `Stream`s.

    'Stream's requiring resource allocation are lifted to values of type
    `Bracketed`, which can be combined using an API that is more restricted
    than that of the original 'Stream's and ensures prompt deallocation of
    resources.
 
    Values of type `Bracketed` can later be run by supplying a
    'Stream'-consumer continuation to the 'with' function.

>>> :{
    do -- boring setup stuff for a two-line text file 
       path <- (</> "streaming-bracketed-doctest.txt") <$> getTemporaryDirectory
       exists <- doesPathExist path
       when exists (removeFile path)
       withFile path WriteMode (for_ ["aaa","bbb"] . hPutStrLn)
       -- end of setup
       let lineStream = R.linesFromFile utf8 nativeNewlineMode path
       lines :> () <- R.with (R.over_ (S.take 1) lineStream *> R.over (S.map (map succ)) lineStream) 
                             S.toList
       return lines
    :}
["aaa","bbb","ccc"]

-}
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

