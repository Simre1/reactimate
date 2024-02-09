{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Reactimate.Game.Assets (Assets, makeAssets, Asset (..), withAsset) where

import Data.HashTable.IO qualified as H
import Data.Hashable (Hashable)
import Data.Proxy
import GHC.Exts (Any)
import Reactimate
import System.Mem.Weak
import Type.Reflection
import Unsafe.Coerce

newtype Assets = Assets (H.LinearHashTable SomeTypeRep (AssetStore Any))

newtype AssetStore k = AssetStore (H.LinearHashTable k (Weak (AssetValue k)))

makeAssets :: IO Assets
makeAssets = Assets <$> H.new

class (Hashable key, Typeable key) => Asset key where
  type AssetValue key
  loadAsset :: key -> IO (AssetValue key)
  freeAsset :: key -> AssetValue key -> IO ()

getAssetStore :: forall key. (Asset key) => Assets -> IO (AssetStore key)
getAssetStore (Assets assets) = do
  let typeKey = someTypeRep $ Proxy @key
  maybeAssetStore <- H.lookup assets typeKey
  case maybeAssetStore of
    Just assetStore -> pure $ unsafeCoerce assetStore
    Nothing -> do
      assetStore <- AssetStore <$> H.new
      H.insert assets typeKey $ unsafeCoerce assetStore
      pure assetStore

lookupAsset :: (Asset key) => Assets -> key -> IO (Maybe (AssetValue key))
lookupAsset assets key = do
  AssetStore assetStore <- getAssetStore assets
  maybeWeakAsset <- H.lookup assetStore key
  case maybeWeakAsset of
    Just weakAsset -> deRefWeak weakAsset
    Nothing -> pure Nothing

insertAsset :: (Asset key) => Assets -> key -> AssetValue key -> IO ()
insertAsset assets key value = do
  (AssetStore assetStore) <- getAssetStore assets
  weakAsset <- mkWeakPtr value (Just $ freeAsset key value)
  H.insert assetStore key weakAsset

-- | Load an asset during the `setup` phase.
withAsset :: forall key a b. (Asset key) => Assets -> key -> (AssetValue key -> Signal a b) -> Signal a b
withAsset assets key = withSetup $ do
  maybeAssetValue <- lookupAsset assets key
  case maybeAssetValue of
    Just asset -> pure asset
    Nothing -> do
      asset <- loadAsset key
      insertAsset assets key asset
      pure asset

-- | An `AssetHandle` is a reference to an asset. An `AssetHandle` may change its referenced asset. For example,
-- an asset might be loaded asynchronically, replacing the default asset with the loaded asset.
-- newtype AssetHandle a = AssetHandle (IO a) deriving (Functor, Applicative, Monad, Semigroup, Monoid)
