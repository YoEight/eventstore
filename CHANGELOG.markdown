0.7.0.0
-------
* Fix date conversion. `recordedEventCreateEpoch` is no longer exposed.
* Add `waitTillCatchup` and `hasCaughtUp` functions.
* Add `exactStream` `ExpectedVersion` smart constructor. As the result,
`ExpectedVersion` constructors are no longer exposed. You have to use
`anyStream`, `noStream`, `emptyStream` or `exactStream` instead.

0.6.0.1
-------
No changes

0.6.0.0
-------
* Support `keepRetrying` reconnection strategy.
