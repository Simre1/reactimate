{-# LANGUAGE TemplateHaskell #-}

module Data.SF.Hyperdrive where

import Control.Monad ((>=>))
import Data.IORef
import GHC.IO (unsafePerformIO)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (Lift)
import Debug.Trace (traceShowId)

newtype SF r a b = SF (r -> Code Q (a -> IO b))

arr :: Code Q (a -> b) -> SF r a b
arr f = SF (const [||pure . $$f||])
{-# INLINE arr #-}

feedback :: (Lift s) => s -> SF r (a, s) (b, s) -> SF r a b
feedback !initial (SF sf) = SF $ \r ->
  [||
  let f = $$(sf r)
      stateRef = unsafePerformIO (newIORef initial)
      {-# NOINLINE stateRef #-}
   in \a -> do
        !s <- readIORef stateRef
        (b, !s') <- f (a, s)
        writeIORef stateRef s'
        pure b
  ||]
{-# INLINE feedback #-}

compile :: SF r a b -> r -> Code Q (a -> IO b)
compile (SF sf) = sf

reactimate :: (() -> IO (Maybe a)) -> IO a
reactimate f = do
  let loop = do
        v <- f ()
        maybe loop pure v
  loop
{-# INLINE reactimate #-}

count :: Int
count = 100000

next :: SF r a b -> SF r b c -> SF r a c
next (SF sf1) (SF sf2) = SF $ \r ->
  [||$$(sf1 r) >=> $$(sf2 r)||]
{-# INLINE next #-}

testFunction :: SF () () (Maybe Int)
testFunction =
  feedback count (arr [||(\((), !x) -> (x - 1, x - 1))||])
    `next` arr [||(\x -> if x == 0 then Just x else Nothing)||]
