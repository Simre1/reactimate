# Reactimate Game

`reactimate-game` is a library for `reactimate` which implements some common functionality for simple 2D games.

## Examples

There are some examples in the `examples` folder. Check them out for a quickstart into the library.

## Rendering

Rendering is done with the `render` signal which renders the given `Picture` with the `Camera` to the screen. The following example renders a red rectangle in the bottom left corner of the screen.
```haskell
import Reactimate
import Reactimate.Game
import qualified Data.Vector.Storable as VS
import Data.Colour.Names

main :: IO ()
main =
  reactimate $
  	limitSampleRate 60 $
	    setupGame (GameConfig "Basic Example" defaultWindow) $ \gameEnv ->
	        constant (Camera (V2 0 0) (V2 800 600), picture) >>> render gameEnv >>> constant Nothing

picture :: Picture
picture = makePicture 0 $ BasicShapes $ VS.singleton $
	ColouredShape (packColour red) (Rectangle (V2 0 0) (V2 500 300))
```

## Reactimate Game Examples


![reactimate-game examples](screenshot.png)
