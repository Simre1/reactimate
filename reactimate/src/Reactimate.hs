-- | @reactimate@ implements signal which are well suited for simulations, game loops and complicated stream processing.
-- It's also possible to react to events as they are happening and integrate them into signal functions.
module Reactimate
  ( Signal,
    unSignal,
    makeSignal,
    runSetup,
    runPureSetup,

    -- * Basic signals
    identity,
    constant,
    arr,
    arr2,
    arrIO,
    actionIO,
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
    feedbackState,
    feedbackLazyState,
    scan,
    delay,

    -- * Switch signals
    caseOf,
    switch,
    rSwitch,

    -- * Signal Setup
    withSetup,
    bracketSetup,

    -- * Time in signals
    Time,
    withTime,
    withFixedTime,
    currentTime,
    deltaTime,
    integrate,

    -- * Random signals,
    generateRandom,
    generateRandomRange,
    generateRandomWithRNG,
    generateRandomRangeWithRNG,

    -- * Run signals
    reactimate,
    sample,
    fold,
    limitSampleRate,
    resample,
    resampleInThread,

    -- * Events
    Event,
  )
where

import Control.Arrow
import Reactimate.Basic
import Reactimate.Event
import Reactimate.Random
import Reactimate.Run
import Reactimate.Sampling
import Reactimate.Setup
import Reactimate.Signal
import Reactimate.Stateful
import Reactimate.Switching
import Reactimate.Time
