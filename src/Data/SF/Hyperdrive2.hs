module Data.SF.Hyperdrive2 where

import Control.Arrow
import Control.Category
import Control.Monad ((>=>))
import Data.IORef
import Data.Vector qualified as V
import Data.Vector.Mutable qualified as VM
import GHC.Exts (Any)
import Unsafe.Coerce
import Prelude hiding ((.))

data SF r a b = SF !(r -> IO [Any]) !(VM.MVector VM.RealWorld Any -> a -> IO b)

instance Category (SF r) where
  id = SF (\_ -> pure []) (\_ a -> pure a)
  (.) = flip next
  {-# INLINE (.) #-}

instance Arrow (SF r) where
  arr f = SF (\_ -> pure []) (\_ a -> pure (f a))
  first = undefined
  {-# INLINE arr #-}

feedback :: s -> SF r (a, s) (b, s) -> SF r a b
feedback !initial (SF setup step) =
  SF
    ( \r -> do
        s <- setup r
        pure $ unsafeCoerce initial : s
    )
    ( \v a -> do
        s <- unsafeCoerce <$> VM.unsafeRead v 0
        ((b, s')) <- step (VM.drop 1 v) (a, s)
        VM.unsafeWrite v 0 $ unsafeCoerce s'
        pure (b)
    )
{-# INLINE feedback #-}

compile :: SF r a b -> r -> IO (a -> IO b)
compile (SF setup step) env = do
  states <- setup env
  stateVec <- V.unsafeThaw $ V.fromList states
  pure $ step stateVec
{-# INLINE compile #-}

reactimate :: IO (Maybe a) -> IO a
reactimate f = do
  let loop = do
        v <- f
        maybe loop pure v
  loop
{-# INLINE reactimate #-}

next :: SF r a b -> SF r b c -> SF r a c
next (SF setup1 step1) (SF setup2 step2) =
  SF
    ( \r -> do
        states1 <- setup1 r
        states2 <- setup2 r
        pure $ states1 ++ states2
    )
    ( \states a -> do
        b <- step1 states a
        c <- step2 states b
        pure c
    )
{-# INLINE next #-}
