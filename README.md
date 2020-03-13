EventStore Haskell TCP client
=============================

[Talk and exchange ideas in our dedicated Discord Server]

That driver supports 100% of EventStore features !
More information about the GetEventStore database can be found there: https://eventstore.org/

Requirements
============
  * 64bits system
  * GHC        >= 8.0.3
  * Cabal      >= 1.18
  * EventStore >= 4 (Doesn't support EventStore 2020 Preview yet, previously named version 6).

*Note: If you use this client version >= to `1.1`, it will only supports EventStore >= 4.0.0.*

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
$ cabal install
```

How to test
===========
Tests are available. Those assume a server is running on `127.0.0.1` and `1113` port.
```
$ cabal test
```

How to use
==========

```haskell
{-# LANGUAGE OverloadedStrings #-} -- That library uses `Text` pervasively. This pragma permits to use
                                   -- String literal when a Text is needed.
module Main where

import Control.Concurrent.Async (wait)
import Data.Aeson
-- It requires to have `aeson` package installed. Note that EventStore doesn't constraint you to JSON
-- format but putting common use aside, by doing so you'll be able to use some interesting EventStore
-- features like its Complex Event Processing (CEP) capabality.

import Database.EventStore
-- Note that imports 'NonEmpty' data constructor and 'nonEmpty' function from
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
    as <- sendEvent conn (StreamName "languages") anyVersion evt Nothing

    -- EventStore interactions are fundamentally asynchronous. Nothing requires you to wait
    -- for the completion of an operation, but it's good to know if something went wrong.
    _ <- wait as

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
That library was tested on Linux and OSX.

Contributions and bug reports are welcome!

BSD3 License

-Yorick Laupa

[Talk and exchange ideas in our dedicated Discord Server]: https://discord.gg/x7q37jJ
