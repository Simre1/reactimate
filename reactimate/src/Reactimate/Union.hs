{-# LANGUAGE UndecidableInstances #-}

module Reactimate.Union where

import Data.Kind

data X

data Union (xs :: [Type -> Type]) where
  EmptyUnion :: Union '[]
  ConsUnion :: x X -> Union xs -> Union (x : xs)

class Member x xs where
  getMember :: Union xs -> x X

instance Member x (x : xs) where
  getMember (ConsUnion x _) = x

instance {-# OVERLAPPABLE #-} (Member x xs) => Member x xs where
  getMember (ConsUnion _ xs) = getMember xs

type e :> es = Member e es
