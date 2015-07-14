module World where

import Entity

import qualified Graphics.Rendering.OpenGL as GL

data World = World {
    entities :: [Entity]
  , width :: Int
  , height :: Int
  , texture :: GL.TextureObject
}
