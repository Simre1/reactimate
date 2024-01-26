-- | @fast-signals@ implements signal which are well suited for simulations, game loops and complicated stream processing.
-- It's also possible to react to events as they are happening and integrate them into signal functions.
module Data.SF
  ( SF,

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

import Data.SF.Basic
import Data.SF.Core
import Data.SF.Delay
import Data.SF.Environment
import Data.SF.Event
import Data.SF.Run
import Data.SF.Sampling
import Data.SF.Stateful
import Data.SF.Switching
import Data.SF.Time
