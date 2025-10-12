{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE TypeFamilies #-}

module Reactimate.Game.Assets where

import Control.Concurrent.STM.Map qualified as STMMap
import Control.Exception.Base
import Control.Monad (forM_, join, when)
import Control.Monad.IO.Class
import Data.Hashable (Hashable (..))
import Data.Kind
import GHC.Conc (atomically)
import GHC.Exts (Any)
import Reactimate
import System.Mem.Weak
import Type.Reflection
import Unsafe.Coerce

-- | `Assets` holds references to your loaded assets (e.g. images)
data Assets s = Assets
  { store :: STMMap.Map SuperKey (Weak (AssetValue Any))
  }

data SuperKey where
  SuperKey :: (Asset key) => TypeRep key -> key -> SuperKey

instance Eq SuperKey where
  (SuperKey rep1 s1) == (SuperKey rep2 s2) = case eqTypeRep rep1 rep2 of
    Nothing -> False
    Just HRefl -> s1 == s2

instance Hashable SuperKey where
  hashWithSalt i (SuperKey _ s) = hashWithSalt i s
  hash (SuperKey _ s) = hash s

-- newtype AssetStore k = AssetStore (H.LinearHashTable k (Weak (AssetValue k)))

makeAssets :: IO (Assets s)
makeAssets = Assets <$> atomically (STMMap.empty)

class (Hashable key, Typeable key) => Asset key where
  -- | The asset you want to load. The key determines the type of the asset.
  type AssetValue key

  -- | The effects you need for loading the asset.
  type AssetEffects key :: [Type -> Type]

  -- | Load an asset with the asset environment and the key.
  loadAsset :: Handles (AssetEffects key) s -> key -> Step s (AssetValue key)

  -- | Free the asset after no one uses it anymore. It is not safe to use effects here!
  freeAsset :: key -> AssetValue key -> IO ()

data AssetHandle key = AssetHandle (IO (Maybe (AssetValue key)))

getAsset :: (Assets :> es, IOE :> es, Asset key, Members (AssetEffects key) es) => key -> Setup es s (AssetValue key)
getAsset key = do
  Assets store <- getHandle
  let superKey = SuperKey typeRep key
  maybeAsset <- liftIO $ do
    foundValue <- atomically $ STMMap.phantomLookup superKey store
    fmap join $ traverse deRefWeak foundValue

  case maybeAsset of
    Just x -> pure $ unsafeCoerce x
    Nothing -> do
      assetEffects <- getHandles

      asset <- prestep $ loadAsset assetEffects key

      weakAsset <- liftIO $ mkWeakPtr asset $ Just $ do
        cleanUpAsset store superKey key asset

      liftIO $ atomically $ STMMap.insert superKey (unsafeCoerce weakAsset) store

      pure asset

lazyGetAsset ::
  (Assets :> es, IOE :> es, Asset key, Members (AssetEffects key) es) =>
  key ->
  Setup es s (Step s (Maybe (AssetValue key)))
lazyGetAsset key = do
  Assets store <- getHandle
  let superKey = SuperKey typeRep key
  maybeAsset <- liftIO $ do
    foundValue <- atomically $ STMMap.phantomLookup superKey store
    fmap join $ traverse deRefWeak foundValue

  case maybeAsset of
    Just x -> pure $ unsafeCoerce x
    Nothing -> do
      IOE lift <- getHandle
      assetEffects <- getHandles
      unlift <- unliftStep

      assetRef <- newRef Nothing

      prestep $ lift $ scheduleLoad $ unlift $ do
        asset <- loadAsset assetEffects key

        weakAsset <- lift $ mkWeakPtr asset $ Just $ do
          cleanUpAsset store superKey key asset

        lift $ atomically $ STMMap.insert superKey (unsafeCoerce weakAsset) store
        writeRef assetRef (Just asset)

      pure $ readRef assetRef

cleanUpAsset ::
  (Asset key) =>
  STMMap.Map SuperKey (Weak (AssetValue Any)) ->
  SuperKey ->
  key ->
  AssetValue key ->
  IO ()
cleanUpAsset store superKey key asset =
  bracket
    ( atomically $ do
        keyFound <- STMMap.member superKey store
        when keyFound $ STMMap.delete superKey store
        pure keyFound
    )
    (\keyFound -> when keyFound $ freeAsset key asset)
    (\_ -> pure ())

-- | TODO: Need to implement lazy load
scheduleLoad :: IO () -> IO ()
scheduleLoad io = io

runAssets :: (IOE :> es) => Signal (Assets : es) a b -> Signal es a b
runAssets = mapEffects $ \setup -> do
  assets@(Assets store) <- liftIO makeAssets
  setup' <- runHandle assets setup
  IOE lift <- getHandle
  Reactimate.finalize $ do
    assetList <- liftIO $ STMMap.unsafeToList store
    forM_ assetList $ \case
      (superKey@(SuperKey @key rep key), weakAsset) -> do
        maybeAsset <- liftIO $ deRefWeak weakAsset
        case maybeAsset of
          Just asset -> prestep $ lift $ cleanUpAsset store superKey key (unsafeCoerce asset)
          Nothing -> pure ()
    pure ()
  pure setup'

-- withAsset :: (Asset key es) => key -> (AssetHandle key -> Signal es a b) -> Signal es a b

-- getAssetStore :: (LoadAsset key es, IOE :> es) => Assets -> IO (AssetStore key)
-- getAssetStore @key (Assets assets) = do
--   let typeKey = someTypeRep $ Proxy @key
--   maybeAssetStore <- H.lookup assets typeKey
--   case maybeAssetStore of
--     Just assetStore -> pure $ unsafeCoerce assetStore
--     Nothing -> do
--       assetStore <- AssetStore <$> H.new
--       H.insert assets typeKey $ unsafeCoerce assetStore
--       pure assetStore

-- lookupAsset :: (LoadAsset key es, IOE :> es) => Assets -> key -> IO (Maybe (AssetValue key))
-- lookupAsset assets key = do
--   AssetStore assetStore <- getAssetStore assets
--   maybeWeakAsset <- H.lookup assetStore key
--   case maybeWeakAsset of
--     Just (weakAsset, _) -> deRefWeak weakAsset
--     Nothing -> pure Nothing

-- insertAsset :: (LoadAsset key es, IOE :> es) => Assets -> key -> AssetValue key -> IO ()
-- insertAsset assets key value = do
--   (AssetStore assetStore) <- getAssetStore assets
--   weakAsset <- liftIO $ mkWeakPtr value $ Just $ do
--     maybeAsset <- lookupAsset assets key
--     case maybeAsset of
--       Just storedValue -> do
--         storedValue
--       -- Freeing already done
--       Nothing -> pure ()
--   H.insert assetStore key weakAsset

-- | Load an asset during the `setup` phase. Getting the same asset with the same
-- key multiple times only loads the asset once and reuses it.
-- withAsset :: (LoadAsset key es) => key -> (AssetValue key -> Signal es a b) -> Signal es a b
-- withAsset assets env key = withSetup $ do
--   maybeAssetValue <- lookupAsset assets key
--   case maybeAssetValue of
--     Just asset -> pure asset
--     Nothing -> do
--       asset <- loadAsset env key
--       insertAsset assets key asset
--       pure asset

-- An `AssetHandle` is a reference to an asset. An `AssetHandle` may change its referenced asset. For example,
-- an asset might be loaded asynchronically, replacing the default asset with the loaded asset.
-- newtype AssetHandle a = AssetHandle (IO a) deriving (Functor, Applicative, Monad, Semigroup, Monoid)
