EventStore Haskell TCP client
=============================

Basically, all we have for now are:

  1. NewEvent
  2. DeleteStream
  3. Transaction
  4. ReadEvent
  5. ReadStreamEvents (Forward and Backward)
  6. ReadAllEvents (Forward and Backward)
  7. Volatile subscriptions

TODO
====

  1. All kind of Subscriptions
  2. Authentication
  3. SSL

Requirements
============
  1. GHC        >= 7.8.3
  2. Cabal      >= 1.20
  3. EventStore >= 3.0.0

(Don't know if it works on Windows)