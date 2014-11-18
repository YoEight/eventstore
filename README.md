Eventstore Haskell TCP client
=============================

WORK IN PROGRESS

Basically, all we have for now are:

  1. Heartbeats (W00t :-))
  2. NewEvent

Requirements
============
  1. GHC        >= 7.8.3
  2. Cabal      >= 1.20
  3. Eventstore >= 3.0.0

(Don't know if it works on Windows)

How to test
===========

```
$ git clone https://github.com/YoEight/eventstore.git
$ cd eventstore
$ cabal sandbox init
$ cabal configure
$ cabal install --only-dependencies
$ cabal build
```