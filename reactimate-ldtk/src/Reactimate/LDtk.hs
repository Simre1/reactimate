module Reactimate.LDtk
  ( withLDtkRoot,
    withLevel,
    LevelName,
    withMatchRules,
    MatchRule,
    Match,
    matchEntity,
    matchTiles,
    matchIntGrid,
    getLayerName,
    getEntityName,
    getEntityPosition,
    getGlobalEntityPosition,
    getEntitySize,
    getEntityField,
    getMatchObject,
    getLayer,
    getLevel,
    module LDtk,
  )
where

import Control.Applicative
import Data.Foldable
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import LDtk
import Reactimate
import Reactimate.Game (V2 (..))

-- import QuickType

-- | Load an `LDtk` file in the setup phase.
withLDtkRoot :: FilePath -> (LDtkRoot -> Signal a b) -> Signal a b
withLDtkRoot filepath =
  withSetup
    ( do
        maybeRoot <- loadLDtk filepath
        either fail pure maybeRoot
    )

type LevelName = Text

-- | Switch to different `Level`s based on their name
withLevel :: LDtkRoot -> LevelName -> (Level -> Signal a (b, Maybe LevelName)) -> Signal a b
withLevel ldtkRoot initialLevel makeSignal = switchRepeatedly (makeSignal $ levels' M.! initialLevel) (makeSignal . (levels' M.!))
  where
    allLevels = ldtkRoot.levels ++ (ldtkRoot.worlds >>= (.levels))
    levels' = M.fromList $ zip ((.identifier) <$> allLevels) allLevels

-- | Apply match rules in sequence to process a level.
withMatchRules :: forall a. (Monoid a) => [MatchRule a] -> Level -> a
withMatchRules rules level =
  flip foldMap level.layerInstances $ \layer ->
    let layerName = layer.__identifier
        matchers ruleMap = fromMaybe (ruleMap M.! Nothing) $ M.lookup (Just layerName) ruleMap
        entities = foldMap (\e -> fromMaybe mempty $ foldr ((<|>) . runMatch level layer e) Nothing (matchers entityRules)) layer.entityInstances
        autoTiles = fromMaybe mempty $ foldr ((<|>) . runMatch level layer layer.autoLayerTiles) Nothing (matchers tileRules)
        gridTiles = fromMaybe mempty $ foldr ((<|>) . runMatch level layer layer.gridTiles) Nothing (matchers tileRules)
        intGrid = fromMaybe mempty $ foldr ((<|>) . runMatch level layer layer.intGridCsv) Nothing (matchers intGridRules)
     in case layer.__type of
          IntGrid -> intGrid
          Entities -> entities
          Tiles -> gridTiles
          AutoLayer -> autoTiles
  where
    entityRules :: M.Map (Maybe Text) [Match Entity a]
    tileRules :: M.Map (Maybe Text) [Match [Tile] a]
    intGridRules :: M.Map (Maybe Text) [Match [Int] a]
    (entityRules, tileRules, intGridRules) = foldl' buildRulesMap (M.empty, M.empty, M.empty) rules
    buildRulesMap (entityRules, tileRules, intGridRules) (MatchRule maybeLayerName someMatch) =
      let alterF ruleMap match rules = Just $ fromMaybe [] (rules <|> M.lookup Nothing ruleMap) ++ [match]
          insert match key ruleMap = M.alter (alterF ruleMap match) key ruleMap
       in case someMatch of
            EntityMatch match -> (insert match Nothing $ insert match maybeLayerName entityRules, tileRules, intGridRules)
            TileMatch match -> (entityRules, insert match Nothing $ insert match maybeLayerName tileRules, intGridRules)
            IntGridMatch match -> (entityRules, tileRules, insert match Nothing $ insert match maybeLayerName intGridRules)
{-# INLINE withMatchRules #-}

data MatchRule a = MatchRule
  { matchedLayerName :: !(Maybe Text),
    match :: !(SomeMatch a)
  }

matchEntity ::
  -- | Layer name filter
  Maybe Text ->
  Match Entity a ->
  MatchRule a
matchEntity layerName = MatchRule layerName . EntityMatch
{-# INLINE matchEntity #-}

matchTiles ::
  -- | Layer name filter
  Maybe Text ->
  Match [Tile] a ->
  MatchRule a
matchTiles layerName = MatchRule layerName . TileMatch
{-# INLINE matchTiles #-}

matchIntGrid ::
  -- | Layer name filter
  Maybe Text ->
  Match [Int] a ->
  MatchRule a
matchIntGrid layerName = MatchRule layerName . IntGridMatch
{-# INLINE matchIntGrid #-}

data SomeMatch a = EntityMatch (Match Entity a) | TileMatch (Match [Tile] a) | IntGridMatch (Match [Int] a)

newtype Match t a = Match ((Level, Layer, t) -> Maybe a) deriving (Functor)

runMatch :: Level -> Layer -> t -> Match t a -> Maybe a
runMatch level layer t (Match f) = f (level, layer, t)
{-# INLINE runMatch #-}

instance Applicative (Match t) where
  pure a = Match $ \_ -> pure a
  {-# INLINE pure #-}
  (Match f1) <*> (Match f2) = Match $ \l -> f1 l <*> f2 l
  {-# INLINE (<*>) #-}

instance Monad (Match t) where
  (Match f1) >>= k = Match $ \l -> do
    a <- f1 l
    let (Match f2) = k a
    f2 l
  {-# INLINE (>>=) #-}

instance Alternative (Match t) where
  (Match f1) <|> (Match f2) = Match $ \t -> f1 t <|> f2 t
  empty = Match $ const Nothing
  {-# INLINE empty #-}
  {-# INLINE (<|>) #-}

instance MonadFail (Match t) where
  fail _ = Match $ const Nothing
  {-# INLINE fail #-}

getLayer :: Match t Layer
getLayer = Match $ \(_, l, _) -> pure l
{-# INLINE getLayer #-}

getLevel :: Match t Level
getLevel = Match $ \(l, _, _) -> pure l
{-# INLINE getLevel #-}

getMatchObject :: Match t t
getMatchObject = Match $ \(_, _, t) -> pure t
{-# INLINE getMatchObject #-}

getLayerName :: Match t Text
getLayerName = Match $ \(_, l, _) -> pure l.__identifier
{-# INLINE getLayerName #-}

getEntityName :: Match Entity Text
getEntityName = Match $ \(_, _, e) -> pure e.__identifier
{-# INLINE getEntityName #-}

-- | Bottom left entity position within a level
getEntityPosition :: Match Entity (V2 Int)
getEntityPosition = Match $ \(_, layer, entity) ->
  let (Pair x y) = entity.px
      (Pair pivotX pivotY) = entity.__pivot
      pivotOffset = fmap (`quot` 2) $ V2 entity.width entity.height * (round <$> (2 * V2 pivotX (pivotY - 1)))
      offset = V2 layer.__pxTotalOffsetX (-layer.__pxTotalOffsetY)
   in pure $ pivotOffset + offset + V2 x (layer.__cHei * layer.__gridSize - y)
{-# INLINE getEntityPosition #-}

getEntitySize :: Match Entity (V2 Int)
getEntitySize = Match $ \(_, _, entity) ->
  pure $ V2 entity.width entity.height
{-# INLINE getEntitySize #-}

-- | Bottom left entity position based on global coordinates respecting level offsets
getGlobalEntityPosition :: Match Entity (V2 Int)
getGlobalEntityPosition = (+) <$> getEntityPosition <*> Match (\(level, _, _) -> pure $ V2 level.worldX (-level.worldY))
{-# INLINE getGlobalEntityPosition #-}

getEntityField :: Text -> Match Entity FieldValue
getEntityField fieldName = do
  entity <- getMatchObject
  maybe (fail "No field with that name") pure $
    lookup fieldName $
      (\field -> (field.__identifier, field.__value)) <$> entity.fieldInstances
{-# INLINE getEntityField #-}
