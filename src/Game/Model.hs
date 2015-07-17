module Game.Model where

import Util.Math

import qualified Graphics.Rendering.OpenGL as GL

data Model = Model {
    points :: [Vector2]
  , texture :: GL.TextureObject
}
