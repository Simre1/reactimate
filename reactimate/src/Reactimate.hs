{-# LANGUAGE PatternSynonyms #-}

-- | @reactimate@ implements signals which are well suited for simulations, game loops and complicated stream processing.
module Reactimate
  ( -- * Signal
    Signal,
    Step,
    Setup,
    makeSignal,
    unSignal,
    finalize,
    prestep,
    unliftStep,
    runPureSetup,
    runSetup,

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
    withSetup,
    bracketSetup,

    -- * Stateful signals
    feedback,
    feedbackState,
    feedbackLazyState,
    scan,
    delay,

    -- ** Mutable references
    Ref,
    newRef,
    writeRef,
    readRef,
    modifyRef,
    modifyRef',
    atomicModifyRef,
    atomicModifyRef',

    -- * Switch signals
    caseOf,
    switch,
    rSwitch,

    -- ** Low-level switch
    Switch,
    newSwitch,
    updateSwitch,
    runSwitch,

    -- * Run signals
    reactimate,
    sample,
    fold,
    limitSampleRate,
    resample,
    resampleInThread,

    -- * Effects
    Handles (..),
    pattern Handle,
    pattern (:->),
    Member,
    (:>),
    Members,
    getHandle,
    getHandles,
    IOE (..),
    runHandle,
    replaceHandle,
    mapEffects,

    -- ** Time in signals
    Time,
    withTime,
    withFixedTime,
    currentTime,
    deltaTime,
    integrate,

    -- ** Random signals,
    RNG,
    generateRandom,
    generateRandomRange,
    runRNGWithIO,
    runRNGWithGenerator,
    generateRandomWithGenerator,
    generateRandomRangeWithGenerator,

    -- * Events
  )
where

-- Event,

import Control.Arrow
import Reactimate.Basic
import Reactimate.Event
import Reactimate.Handles
import Reactimate.Random
import Reactimate.Run
import Reactimate.Sampling
import Reactimate.Setup
import Reactimate.Signal
import Reactimate.Stateful
import Reactimate.Switching
import Reactimate.Time
