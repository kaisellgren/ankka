module Game.Scene where

import Game.World
import Util.Math

data Scene = Scene {
    world :: World
  , cameraPosition :: Vector2
}
