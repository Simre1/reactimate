{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE UndecidableInstances #-}

module Reactimate.Handles
  ( Handles (..),
    pattern (:->),
    pattern Handle,
    Member (..),
    (:>),
    Members (..),
  )
where

import Data.Kind

-- | Stores the effect handlers in a heterogenous list
data Handles (xs :: [Type -> Type]) s where
  NoHandles :: Handles '[] s
  ConsHandle :: x s -> Handles xs s -> Handles (x : xs) s

-- | Convenient synonym for 'ConsHandle'
pattern (:->) :: x s -> Handles xs s -> Handles (x : xs) s
pattern (:->) a b = ConsHandle a b

{-# COMPLETE (:->) #-}

-- | Match a single effect handle
pattern Handle :: x s -> Handles '[x] s
pattern Handle a = ConsHandle a NoHandles

{-# COMPLETE Handle #-}

-- | Access for 'Handles' based on a type index
class Member x xs where
  getMember :: Handles xs s -> x s
  replaceMember :: x s -> Handles xs s -> Handles xs s

instance Member x (x : xs) where
  getMember (ConsHandle x _) = x
  replaceMember x (ConsHandle _ xs) = ConsHandle x xs

instance {-# OVERLAPPABLE #-} (Member x xs) => Member x (y : xs) where
  getMember (ConsHandle _ xs) = getMember xs
  replaceMember x (ConsHandle y xs) = ConsHandle y $ replaceMember x xs

-- | Convenient shorthand for 'Member'
type e :> es = Member e es

-- | Extracts a sub list of effect handles from 'Handles'
class Members sub xs where
  getMembers :: Handles xs s -> Handles sub s

instance (Members subs xs, Member x xs) => Members (x : subs) xs where
  getMembers union = getMember union `ConsHandle` getMembers union

instance Members '[] xs where
  getMembers _ = NoHandles
