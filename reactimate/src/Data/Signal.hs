-- | @reactimate@ implements signal which are well suited for simulations, game loops and complicated stream processing.
-- It's also possible to react to events as they are happening and integrate them into signal functions.
module Data.Signal
  ( Signal,

    -- * Basic signals
    identity,
    constant,
    arr2,
    arrIO,
    dup,

    -- * Stateful signals
    feedback,
    lazyFeedback,
    scan,

    -- * Delay signals
    delaySample,

    -- * Switch signals
    caseOf,
    switch,

    -- * Signal Environemnt
    useEnv,
    modifyEnv,
    mapEnv,
    withSetup,

    -- * Time in signals
    Time,
    withTime,
    withFixedTime,
    currentTime,
    deltaTime,
    integrate,

    -- * Run signals
    reactimate,
    sample,
    fold,
    limitSampleRate,
    resample,
    resampleInThread,

    -- * Events
    Event,
    Behavior,
    pulse,
    callback,
    eventMap,
    holdEvent,

    -- ** Sampling events
    accumulateEvent,
    sampleEvent,
    sampleEventAsList,
    sampleBehavior,
  )
where

import Data.Signal.Basic
import Data.Signal.Core
import Data.Signal.Delay
import Data.Signal.Environment
import Data.Signal.Event
import Data.Signal.Run
import Data.Signal.Sampling
import Data.Signal.Stateful
import Data.Signal.Switching
import Data.Signal.Time
