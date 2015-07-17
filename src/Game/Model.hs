module Model where

import Math

import qualified Graphics.Rendering.OpenGL as GL

data Model = Model {
    points :: [Vector2]
  , texture :: GL.TextureObject
}
