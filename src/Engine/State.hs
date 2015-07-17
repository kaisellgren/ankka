module Engine.State where

import Game.Scene
import Engine.Input

data State = State
    { stateWindowWidth     :: !Int
    , stateWindowHeight    :: !Int
    , frameNumber          :: !Int
    , prevTime             :: !Double
    , scene                :: !Scene
    , input                :: !Input
    }
