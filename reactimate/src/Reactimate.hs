-- | @reactimate@ implements signal which are well suited for simulations, game loops and complicated stream processing.
-- It's also possible to react to events as they are happening and integrate them into signal functions.
module Reactimate
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

    -- * Signal Setup
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

import Reactimate.Basic
import Reactimate.Delay
import Reactimate.Environment
import Reactimate.Event
import Reactimate.Run
import Reactimate.Sampling
import Reactimate.Signal
import Reactimate.Stateful
import Reactimate.Switching
import Reactimate.Time
