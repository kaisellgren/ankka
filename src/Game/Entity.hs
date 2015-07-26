module Game.Entity where

import Engine.Graphics
import Game.Model
import Util.Vector2

import qualified Graphics.Rendering.OpenGL as GL

data Entity = Entity {
    angle :: Float
  , model :: Model
  , velocity :: Vector2
  , position :: Vector2
}

createEntity :: Entity
createEntity = Entity
  { angle = 0
  , position = Vector2 0 0
  , velocity = Vector2 0 0
  , model = undefined
  }

renderEntity :: Entity -> IO ()
renderEntity e = do
    GL.activeTexture GL.$= GL.TextureUnit 0
    GL.textureBinding GL.Texture2D GL.$= Just (texture $ model e)
    renderRectangle $ fmap (vadd $ position e) (points (model e))
