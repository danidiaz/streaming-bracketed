# streaming-bracketed

From the [CHANGELOG](http://hackage.haskell.org/package/streaming-0.2.1.0/changelog) of the [streaming](http://hackage.haskell.org/package/streaming) package:

> Remove bracketStream, MonadCatch instance, and everything dealing with
> ResourceT. All of these things of sort of broken for Stream since there is no
> guarantee of linear consumption (functions like take can prevent finalizers
> from running).

[One Github issue](https://github.com/haskell-streaming/streaming/issues/52). [Another one](https://github.com/haskell-streaming/streaming-with/issues/2).

[Streaming libs exercise/challenge](https://twitter.com/DiazCarrete/status/1016073374458671104):

> Given a list [(Filepath,Int,Int)] of files and line ranges, create a stream
> of lines belonging to the concatenated ranges.

> Prompt release of file handles is required. resource-handling monads and
> "withXXX"-style functions are allowed.


