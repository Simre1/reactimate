# Reactimate LDtk

Load LDtk files and integrate them into your `reactimate` based games. It also has a matching framework to extract level information in a convenient way.

## Examples

The following example loads `Level_0` from the project file. The `MatchRule Picture` extracts all entities from the level and generated a `Picture` with a red rectangle at their position.

```haskell
renderLDtk :: FilePath -> Signal () Picture
renderLDtk filepath = withLDtkRoot filepath $ \ldtkRoot -> withLevel ldtkRoot "Level_0" $ \level ->
  constant (withMatchRules rules level) >>> arr (,Nothing)
  where
    rules :: [MatchRule Picture]
    rules = pure $ matchEntity Nothing $ do
      entitySize <- getEntitySize
      entityPosition <- getEntityPosition
      pure $
        makePicture 0 $
          BasicShapes $
            VS.singleton
              ( ColouredShape (packColour red) $
                  BSRectangle $
                    Rectangle entityPosition entitySize
              )
```


