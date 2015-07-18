module Game.Model where

import Util.Vector2

import qualified Graphics.Rendering.OpenGL as GL

data Model = Model {
    points :: [Vector2]
  , texture :: GL.TextureObject
}
