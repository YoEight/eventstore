EventStore Haskell TCP client
=============================

[![Join the chat at https://gitter.im/YoEight/eventstore](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/YoEight/eventstore?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)
[![Build Status](https://travis-ci.org/YoEight/eventstore.svg?branch=master)](https://travis-ci.org/YoEight/eventstore)

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
