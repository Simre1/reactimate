-- | @reactimate@ implements signal which are well suited for simulations, game loops and complicated stream processing.
-- It's also possible to react to events as they are happening and integrate them into signal functions.
module Reactimate
  ( Signal,

    -- * Basic signals
    identity,
    constant,
    arr,
    arr2,
    arrIO,
    dup,

    -- * Combinators
    (>>>),
    (<<<),
    (&&&),
    (***),
    first,
    second,
    (|||),
    (+++),
    left,
    right,
    
    -- * Stateful signals
    feedback,
    lazyFeedback,
    scan,

    -- * Delay signals
    delaySample,
    once,

    -- * Switch signals
    caseOf,
    switch,

    -- * Signal Setup
    withSetup,
    allocateResource,
    addFinalizer,

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
    Dynamic,
    makeBehavior,
    pulse,
    callback,
    mapEvent,
    mapBehavior,
    holdEvent,

    -- ** Sampling events
    accumulateEvent,
    sampleEvent,
    sampleEventAsList,
    sampleBehavior,
    sampleDynamic
  )
where

import Control.Arrow
import Reactimate.Basic
import Reactimate.Delay
import Reactimate.Setup
import Reactimate.Event
import Reactimate.Run
import Reactimate.Sampling
import Reactimate.Signal
import Reactimate.Stateful
import Reactimate.Switching
import Reactimate.Time
