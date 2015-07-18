module Game.Entity where

import Engine.Graphics
import Game.Model
import Util.Math

import qualified Graphics.Rendering.OpenGL as GL

data Entity = Entity {
    angle :: Float
  , model :: Model
  , velocity :: Vector2
  , position :: Vector2
}

updateEntity :: Float -> Entity -> Entity
updateEntity deltaTime e = e
    { position = vadd (position e) (vscale (velocity e) deltaTime)
    }

renderEntity :: Entity -> IO ()
renderEntity e = do
    GL.activeTexture GL.$= GL.TextureUnit 0
    GL.textureBinding GL.Texture2D GL.$= Just (texture $ model e)
    renderRectangle $ fmap (vadd $ position e) (points (model e))
