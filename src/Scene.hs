module Scene where

import World
import Math

data Scene = Scene {
    world :: World
  , cameraPosition :: Vector2
}
