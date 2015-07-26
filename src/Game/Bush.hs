module Game.Bush where

import Game.Entity
import Util.Vector2

data Bush = Bush {
    entity :: Entity
  , visualCover :: Float
}

updateBush :: Float -> Bush -> Bush
updateBush deltaTime bush = bush
    { entity = e
      { position = vadd (position e) (vscale (velocity e) deltaTime)
      }
    }
  where e = entity bush
