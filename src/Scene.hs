module Scene where

import World

data Scene = Scene {
    world :: World
  , camera :: Camera
}
