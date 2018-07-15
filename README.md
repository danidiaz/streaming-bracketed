# streaming-bracketed

## What's this?

A resource management "decorator" for the `Stream` type from
[streaming](http://hackage.haskell.org/package/streaming).  

The idea is that the `Bracketed` type represents a `Stream` which might have
some finalizers that will be triggered when we reach a given point in the
stream.

By being careful about how we lift operations to work on `Bracketed` streams,
we can ensure that finalizers are promptly called even with operations like
`take`.

## Differences with resourcet

- Only works on `Stream`s from streaming.

- It sits above the streaming monad, not below.

- Less versatile: finalizer scopes are nested, unlike resourcet which allows
  arbitrary interleavings.

- Smarter handling of functions like `take`.

## Doubts

- Lifting functions like `splitAt` might cause problems if we try to use the
  rest of the stream.

## motivation

From the
[CHANGELOG](http://hackage.haskell.org/package/streaming-0.2.1.0/changelog) of
the [streaming](http://hackage.haskell.org/package/streaming) package:

> Remove bracketStream, MonadCatch instance, and everything dealing with
> ResourceT. All of these things of sort of broken for Stream since there is no
> guarantee of linear consumption (functions like take can prevent finalizers
> from running).

[One Github issue](https://github.com/haskell-streaming/streaming/issues/52).
[Another one](https://github.com/haskell-streaming/streaming-with/issues/2).

[Streaming libs exercise/challenge](https://twitter.com/DiazCarrete/status/1016073374458671104):

> Given a list [(Filepath,Int,Int)] of files and line ranges, create a stream
> of lines belonging to the concatenated ranges.

> Prompt release of file handles is required. resource-handling monads and
> "withXXX"-style functions are allowed.

