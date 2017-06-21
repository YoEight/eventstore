EventStore Haskell TCP client
=============================

[![Join the chat at https://gitter.im/YoEight/eventstore](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/YoEight/eventstore?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)
[![Build Status](https://travis-ci.org/YoEight/eventstore.svg?branch=dev%2F0.15)](https://travis-ci.org/YoEight/eventstore)

That driver supports 100% of EventStore features !
More information about the GetEventStore database can be found there: https://geteventstore.com/

Requirements
============
  * 64bits system
  * GHC        >= 7.8.3
  * Cabal      >= 1.18
  * EventStore >= 3.0.0 (>= 3.1.0 if you want competing consumers)

Install
=======

* Using [Hackage](https://hackage.haskell.org/package/eventstore)
```
$ cabal update
$ cabal install eventstore
```

* From source
```
$ git clone https://github.com/YoEight/eventstore.git
$ cd eventstore
$ cabal install --only-dependencies
$ cabal configure
$ cabal install
```

How to test
===========
Tests are available. Those assume a server is running on `127.0.0.1` and `1113` port.
```
$ cabal install --only-dependencies --enable-tests
$ cabal configure --enable-tests
$ cabal test
```

How to use
==========

```haskell
{-# LANGUAGE OverloadedStrings #-} -- That library uses `Text` pervasively. This pragma permits to use
                                   -- String literal when a Text is needed.
module Main where

import Data.Aeson
-- It requires to have `aeson` package installed. Note that EventStore doesn't constraint you to JSON
-- format but putting common use aside, by doing so you'll be able to use some interesting EventStore
-- features like its Complex Event Processing (CEP) capabality.

import Database.EventStore
-- Note that import also re-exports 'waitAsync'
-- function for instance. There are also 'NonEmpty' data constructor and 'nonEmpty' function from
-- 'Data.List.NonEmpty'.

main :: IO ()
main = do
    -- A common pattern with an EventStore connection is to create a single instance only and pass it
    -- wherever you need it (it's threadsafe). It's very important to not consider an EventStore connection like
    -- its regular SQL counterpart. An EventStore connection will try its best to reconnect
    -- automatically to the server if the connection dropped. Of course that behavior can be tuned
    -- through some settings.
    conn <- connect defaultSettings (Static "127.0.0.1" 1113)
    let js  = object ["isHaskellTheBest" .= True] -- (.=) comes from Data.Aeson module.
        evt = createEvent "programming" Nothing (withJson js)

    -- Appends an event to a stream named `languages`.
    as <- sendEvent conn "languages" anyVersion evt

    -- EventStore interactions are fundamentally asynchronous. Nothing requires you to wait
    -- for the completion of an operation, but it's good to know if something went wrong.
    _ <- waitAsync as

    -- Again, if you decide to `shutdown` an EventStore connection, it means your application is
    -- about to terminate.
    shutdown conn

    -- Make sure the EventStore connection completes every ongoing operation. For instance, if
    -- at the moment we call `shutdown` and some operations (or subscriptions) were still pending,
    -- the connection aborted all of them.
    waitTillClosed conn
```
Notes
=====
That library was tested on Linux and OSX Yosemite.

Contributions and bug reports are welcome!

BSD3 License

-Yorick Laupa
