# fusers

This repository contains in-progress work on a Haskell effectful streaming library that *guarantees* fusion
statically with staging. This should enable programmers to write performant and effectful stream processing code *without* having to rely on GHC's rewriting system for fusion.

The ideas in this repository are not new, but as far as I can tell, this is the first project to try to glue them all together. The main inspiration comes from:

1. [Stream Fusion](https://www.cs.tufts.edu/~nr/cs257/archive/duncan-coutts/stream-fusion.pdf) and [Streamly](https://hackage.haskell.org/package/streamly-core). The first presents as state-machine model of pure (pull) streams, and the second extends this concept to to effectful streams.
2. [Stream Fusion, To Completeness](https://arxiv.org/abs/1612.06668), which shows how to use staging/program generation to eliminate the overhead of pull streams without relying on rewrite rules.
3. [Closure-Free Programming in 2-Level Type Theory](https://andraskovacs.github.io/pdfs/2ltt_icfp24.pdf), and in particular [Monadic Binding-Time Improvement](https://github.com/AndrasKovacs/staged/blob/main/icfp24paper/supplement/haskell-cftt/CFTT/Improve.hs) from Section 3.4, which shows how to further fuse monadic actions. Similarly, [this project](https://github.com/AndrasKovacs/staged-fusion) (also by Andras Kovacs) demonstrates a similar idea, albeit without effects.
