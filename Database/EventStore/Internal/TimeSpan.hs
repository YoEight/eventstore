{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------------------
-- |
-- Module : Database.EventStore.Internal.TimeSpan
-- Copyright : (C) 2015 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
-- .NET TimeSpan implemented in Haskell.
--------------------------------------------------------------------------------
module Database.EventStore.Internal.TimeSpan
    ( TimeSpan
    , timeSpanTicks
    , timeSpanHoursMinsSecs
    , timeSpanDaysHoursMinsSecs
    , timeSpanDaysHoursMinsSecsMillis
    , timeSpanGetTicks
    , timeSpanGetDays
    , timeSpanGetHours
    , timeSpanGetMinutes
    , timeSpanGetSeconds
    , timeSpanGetMillis
    , timeSpanFromSeconds
    , timeSpanFromMinutes
    , timeSpanFromHours
    , timeSpanFromDays
    , timeSpanTotalDays
    , timeSpanTotalHours
    , timeSpanTotalMinutes
    , timeSpanTotalSeconds
    , timeSpanTotalMillis
    ) where

--------------------------------------------------------------------------------
import Data.Int
import Data.Monoid
import Prelude

--------------------------------------------------------------------------------
import Data.Text.Lazy (unpack)
import Data.Text.Lazy.Builder

--------------------------------------------------------------------------------
-- | .NET TimeSpan: Represents a time interval.
newtype TimeSpan = TimeSpan Int64 deriving (Eq, Ord)

--------------------------------------------------------------------------------
instance Show TimeSpan where
    show = unpack . toLazyText . timeSpanBuilder

--------------------------------------------------------------------------------
millisPerSecond :: Int64
millisPerSecond = 1000

--------------------------------------------------------------------------------
millisPerMinute :: Int64
millisPerMinute = millisPerSecond * 60

--------------------------------------------------------------------------------
millisPerHour :: Int64
millisPerHour = millisPerMinute * 60

--------------------------------------------------------------------------------
millisPerDay :: Int64
millisPerDay = millisPerHour * 24

--------------------------------------------------------------------------------
ticksPerMillisecond :: Int64
ticksPerMillisecond = 10000

--------------------------------------------------------------------------------
ticksPerSecond :: Int64
ticksPerSecond = ticksPerMillisecond * 1000

--------------------------------------------------------------------------------
ticksPerMinute :: Int64
ticksPerMinute = ticksPerSecond * 60

--------------------------------------------------------------------------------
ticksPerHour :: Int64
ticksPerHour = ticksPerMinute * 60

--------------------------------------------------------------------------------
ticksPerDay :: Int64
ticksPerDay = ticksPerHour * 24

--------------------------------------------------------------------------------
daysPerTick :: Double
daysPerTick = 1 / (realToFrac ticksPerDay)

--------------------------------------------------------------------------------
hoursPerTick :: Double
hoursPerTick = 1 / (realToFrac ticksPerHour)

--------------------------------------------------------------------------------
minutesPerTick :: Double
minutesPerTick = 1 / (realToFrac ticksPerMinute)

--------------------------------------------------------------------------------
secondsPerTick :: Double
secondsPerTick = 1 / (realToFrac ticksPerSecond)

--------------------------------------------------------------------------------
millisPerTick :: Double
millisPerTick = 1 / (realToFrac ticksPerMillisecond)

--------------------------------------------------------------------------------
maxMillis :: Int64
maxMillis =
    truncate
    (((realToFrac (maxBound :: Int64) :: Double)
      / realToFrac ticksPerMillisecond) :: Double)

--------------------------------------------------------------------------------
minMillis :: Int64
minMillis =
    truncate
    (((realToFrac (minBound :: Int64) :: Double)
      / realToFrac ticksPerMillisecond) :: Double)

--------------------------------------------------------------------------------
-- | Initializes a new instance of the TimeSpan structure to the specified
--   number of ticks.
timeSpanTicks :: Int64 -> TimeSpan
timeSpanTicks = TimeSpan

--------------------------------------------------------------------------------
-- | Initializes a new instance of the TimeSpan structure to a specified number
--   of hours, minutes, and seconds.
timeSpanHoursMinsSecs :: Int64 -> Int64 -> Int64 -> TimeSpan
timeSpanHoursMinsSecs hh mm ss = TimeSpan $ totalSecs * ticksPerSecond
  where
    totalSecs = (hh * 3600) + (mm * 60) + ss

--------------------------------------------------------------------------------
-- | Initializes a new instance of the TimeSpan structure to a specified number
--   of days, hours, minutes, and seconds.
timeSpanDaysHoursMinsSecs :: Int64 -> Int64 -> Int64 -> Int64 -> TimeSpan
timeSpanDaysHoursMinsSecs dd hh mm ss =
    timeSpanDaysHoursMinsSecsMillis dd hh mm ss 0

--------------------------------------------------------------------------------
-- | Initializes a new instance of the TimeSpan structure to a specified number
--   of days, hours, minutes, seconds, and milliseconds.
timeSpanDaysHoursMinsSecsMillis :: Int64
                                -> Int64
                                -> Int64
                                -> Int64
                                -> Int64
                                -> TimeSpan
timeSpanDaysHoursMinsSecsMillis dd hh mm ss ms =
    TimeSpan $ totalMillis * ticksPerMillisecond
    where
      totalMillis = ((dd * 3600 * 24) +
                    (hh * 3600)       +
                    (mm * 60)         +
                    ss) * 1000 + ms

--------------------------------------------------------------------------------
-- | Gets the number of ticks that represent the value of the current 'TimeSpan'
--   structure.
timeSpanGetTicks :: TimeSpan -> Int64
timeSpanGetTicks (TimeSpan i) = i

--------------------------------------------------------------------------------
-- | Gets the days component of the time interval represented by the current
--   'TimeSpan' structure.
timeSpanGetDays :: TimeSpan -> Int64
timeSpanGetDays (TimeSpan i) = truncate $
                               (realToFrac i :: Double) /
                               (realToFrac ticksPerDay)

--------------------------------------------------------------------------------
-- | Gets the hours component of the time interval represented by the current
--   'TimeSpan' structure.
timeSpanGetHours :: TimeSpan -> Int64
timeSpanGetHours (TimeSpan i) = mod (truncate $
                                (realToFrac i :: Double) /
                                (realToFrac ticksPerHour)) 24

--------------------------------------------------------------------------------
-- | Gets the minutes component of the time interval represented by the current
--   'TimeSpan' structure.
timeSpanGetMinutes :: TimeSpan -> Int64
timeSpanGetMinutes (TimeSpan i) = mod (truncate $
                                  (realToFrac i :: Double) /
                                  (realToFrac ticksPerMinute)) 60

--------------------------------------------------------------------------------
-- | Gets the seconds component of the time interval represented by the current
--   'TimeSpan' structure.
timeSpanGetSeconds :: TimeSpan -> Int64
timeSpanGetSeconds (TimeSpan i) = mod (truncate $
                                  (realToFrac i :: Double) /
                                  (realToFrac ticksPerSecond)) 60

--------------------------------------------------------------------------------
-- | Gets the milliseconds component of the time interval represented by the
--   current 'TimeSpan' structure.
timeSpanGetMillis :: TimeSpan -> Int64
timeSpanGetMillis (TimeSpan i) = mod (truncate $
                                 (realToFrac i :: Double) /
                                 (realToFrac ticksPerMillisecond)) 1000

--------------------------------------------------------------------------------
-- | Returns a 'TimeSpan' that represents a specified number of seconds, where
--   the specification is accurate to the nearest millisecond.
timeSpanFromSeconds :: Double -> TimeSpan
timeSpanFromSeconds i = interval i millisPerSecond

--------------------------------------------------------------------------------
-- | Returns a 'TimeSpan' that represents a specified number of minutes, where
--   the specification is accurate to the nearest millisecond.
timeSpanFromMinutes :: Double -> TimeSpan
timeSpanFromMinutes i = interval i millisPerMinute

--------------------------------------------------------------------------------
-- | Returns a 'TimeSpan' that represents a specified number of hours, where the
--   specification is accurate to the nearest millisecond.
timeSpanFromHours :: Double -> TimeSpan
timeSpanFromHours i = interval i millisPerHour

--------------------------------------------------------------------------------
-- | Returns a 'TimeSpan' that represents a specified number of days, where the
--   specification is accurate to the nearest millisecond.
timeSpanFromDays :: Double -> TimeSpan
timeSpanFromDays i = interval i millisPerDay

--------------------------------------------------------------------------------
-- | Gets the value of the current 'TimeSpan' structure expressed in whole and
--   fractional days.
timeSpanTotalDays :: TimeSpan -> Double
timeSpanTotalDays (TimeSpan i) = (realToFrac i) * daysPerTick

--------------------------------------------------------------------------------
-- | Gets the value of the current 'TimeSpan' structure expressed in whole and
--   fractional hours.
timeSpanTotalHours :: TimeSpan -> Double
timeSpanTotalHours (TimeSpan i) = (realToFrac i) * hoursPerTick

--------------------------------------------------------------------------------
-- | Gets the value of the current 'TimeSpan' structure expressed in whole and
--   fractional minutes.
timeSpanTotalMinutes :: TimeSpan -> Double
timeSpanTotalMinutes (TimeSpan i) = (realToFrac i) * minutesPerTick

--------------------------------------------------------------------------------
-- | Gets the value of the current 'TimeSpan' structure expressed in whole and
--   fractional seconds.
timeSpanTotalSeconds :: TimeSpan -> Double
timeSpanTotalSeconds (TimeSpan i) = (realToFrac i) * secondsPerTick

--------------------------------------------------------------------------------
-- | Gets the value of the current 'TimeSpan' structure expressed in whole and
--   fractional milliseconds.
timeSpanTotalMillis :: TimeSpan -> Double
timeSpanTotalMillis (TimeSpan i) =
    let tmp = (realToFrac i) * millisPerTick in
    if tmp > (realToFrac maxMillis) then realToFrac maxMillis
    else if tmp < (realToFrac minMillis) then realToFrac minMillis
         else tmp

--------------------------------------------------------------------------------
data FormatLiteral = Positive | Negative

--------------------------------------------------------------------------------
padded :: Int -> a -> [a] -> [a]
padded n p xs = replicate diff p ++ xs
  where
    len_xs = length xs
    diff   = n - len_xs

--------------------------------------------------------------------------------
timeSpanBuilder :: TimeSpan -> Builder
timeSpanBuilder (TimeSpan ticks) =
    start    <>
    genDay   <>
    genHours <>
    genMins  <>
    genSecs  <>
    genFract

  where
    ticksPerHourD   = realToFrac ticksPerHour   :: Double
    ticksPerDayD    = realToFrac ticksPerDay    :: Double
    ticksPerMinuteD = realToFrac ticksPerMinute :: Double
    ticksPerSecondD = realToFrac ticksPerSecond :: Double

    day :: Int64
    day = truncate $ realToFrac ticks / ticksPerDayD

    time = ticks `mod` ticksPerDay

    cday  = if ticks < 0 then negate day else day
    ctime = if ticks < 0 then negate time else time

    hours :: Int64
    hours = mod (truncate (realToFrac ctime / ticksPerHourD)) 24

    mins :: Int64
    mins = mod (truncate (realToFrac ctime / ticksPerMinuteD)) 60

    secs :: Int64
    secs = mod (truncate (realToFrac ctime / ticksPerSecondD)) 60

    fraction :: Int64
    fraction = ctime `mod` ticksPerSecond

    literal = if ticks < 0 then Negative else Positive

    start =
        case literal of
            Positive -> fromText ""
            Negative -> fromText "-"

    genDay =
        if cday /= 0
        then fromString (show cday) <> fromText "."
        else mempty

    genHours = fromString (padded 2 '0' $ show hours) <> ":"
    genMins  = fromString (padded 2 '0' $ show mins) <> ":"
    genSecs  = fromString (padded 2 '0' $ show secs)

    genFract =
        if fraction /= 0
        then fromText "." <> fromString (padded 7 '0' $ show fraction)
        else mempty

--------------------------------------------------------------------------------
interval :: Double -> Int64 -> TimeSpan
interval value scale =
    let tmp    = value * (realToFrac scale)
        millis = tmp + (if value >= 0 then 0.5 else (-0.5))
        res    = truncate (millis * (realToFrac ticksPerMillisecond)) in
    TimeSpan res
