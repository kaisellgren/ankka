module Entity where

import Model
import Math

data Entity = Entity {
    angle :: Float
  , model :: Model
  , velocity :: Vector2
  , position :: Vector2
}
