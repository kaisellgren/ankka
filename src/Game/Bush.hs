module Game.Bush where

import Game.Entity
import Game.Model
import Util.Vector2

import qualified Graphics.Rendering.OpenGL as GL

data Bush = Bush {
    entity :: Entity
  , visualCover :: Float
}

createBush :: Vector2 -> GL.TextureObject -> Bush
createBush position texture = Bush
    { entity = createEntity
      { position = position
      , model = Model
        { points = [Vector2 0 0, Vector2 128 0, Vector2 128 128, Vector2 0 128]
        , Game.Model.texture = texture
        }
      }
    , visualCover = 0.5
    }

updateBush :: Float -> Bush -> Bush
updateBush deltaTime bush = bush
    { entity = e
      { position = vadd (position e) (vscale (velocity e) deltaTime)
      }
    }
  where e = entity bush
