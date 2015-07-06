module Entity where

import Model
import Math

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
