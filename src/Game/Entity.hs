module Game.Entity where

import Game.Model
import Util.Math

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
