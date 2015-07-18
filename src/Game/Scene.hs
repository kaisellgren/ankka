module Game.Scene where

import Game.World
import Util.Vector2

data Scene = Scene {
    world :: World
  , cameraPosition :: Vector2
}
