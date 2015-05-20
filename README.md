EventStore Haskell TCP client
=============================

[![Join the chat at https://gitter.im/YoEight/eventstore](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/YoEight/eventstore?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)
[![Build Status](https://travis-ci.org/YoEight/eventstore.svg?branch=master)](https://travis-ci.org/YoEight/eventstore)

That driver supports:

  1. Read event(s) from regular or $all stream (forward or backward).
  2. Write event(s) to regular stream.
  3. Delete regular stream.
  4. Transactional writes to regular stream.
  5. Volatile subscriptions to regular or $all stream.
  6. Catch-up subscriptions to regular or $all stream.
  7. Competing consumers (a.k.a Persistent subscriptions) to regular stream.
  8. Authenticated communication with EventStore server.
  9. Read stream metadata (ACL and custom properties).
  10. Write stream metadata (ACL and custom properties).

TODO
====
  1. SSL

Requirements
============
  1. GHC        >= 7.8.3
  2. Cabal      >= 1.18
  3. EventStore >= 3.0.0 (>= 3.1.0 if you want competing consumers)

Tested on Linux and OSX Yosemite.

BSD3 License
