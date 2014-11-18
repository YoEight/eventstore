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
We're still in dev mode, so here's a easy way to play with the library

```
$ git clone https://github.com/YoEight/eventstore.git
$ cd eventstore
$ cabal sandbox init
$ cabal configure
$ cabal install --only-dependencies
$ cabal build
```

Now, let's consider for example that Testing.hs file, located at project root

```haskell
module Testing where

import Database.Eventstore

test :: IO ()
test = do mgr <- eventStoreConnect defaultSettings "127.0.0.1" 1113
          putStrLn "Press key to continue..."
          getLine
          eventStoreShutdown mgr
```

In the project root

```
cabal repl
```

You will enter in GHCi shell with all needed dependencies loaded. At the end, you should have this:

```
*Database.Eventstore>
```

Enter this.

```
:l Testing.hs
```

You should get

```
*Testing>
```

Supposing you have already started your Eventstore server, enter this

```
test
```

Logs are flooding your terminal now.

```
*Testing> test
Press key to continue...
Connected 18c5b189-5256-4827-b391-4f3f536f68a8
HeartbeatRequest 2ec4f438-095a-0a43-a179-8781f166083e
Send command HeartbeatResponse 2ec4f438-095a-0a43-a179-8781f166083e
HeartbeatRequest 5bd9508f-b010-b340-8aba-325c8b08d6da
Send command HeartbeatResponse 5bd9508f-b010-b340-8aba-325c8b08d6da
HeartbeatRequest 63fbae71-1365-354e-8ea9-e8641ca1b396
Send command HeartbeatResponse 63fbae71-1365-354e-8ea9-e8641ca1b396
HeartbeatRequest a1010ef4-e4bf-d44f-b6ee-88aa19af95ce
Send command HeartbeatResponse a1010ef4-e4bf-d44f-b6ee-88aa19af95ce
HeartbeatRequest 574f2ebc-a4d8-7b4a-8dc8-a58019098014
Send command HeartbeatResponse 574f2ebc-a4d8-7b4a-8dc8-a58019098014
...
```