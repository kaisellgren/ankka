module Game.World where

import Game.Entity
import Game.Bush

import qualified Graphics.Rendering.OpenGL as GL

data World = World {
    bushEntities :: [Bush]
  , width :: Int
  , height :: Int
  , texture :: GL.TextureObject
}
