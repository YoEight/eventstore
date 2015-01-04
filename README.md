EventStore Haskell TCP client
=============================
[![Build Status](https://travis-ci.org/YoEight/eventstore.svg)](https://travis-ci.org/YoEight/eventstore)

Basically, all we have for now are:

  1. NewEvent
  2. DeleteStream
  3. Transaction
  4. ReadEvent
  5. ReadStreamEvents (Forward and Backward)
  6. ReadAllEvents (Forward and Backward)
  7. Volatile subscriptions
  8. Authentication
  9. Catchup subscriptions

TODO
====

  1. Persistent Subscriptions
  2. SSL

Requirements
============
  1. GHC        >= 7.8.3
  2. Cabal      >= 1.20
  3. EventStore >= 3.0.0

(Don't know if it works on Windows)
