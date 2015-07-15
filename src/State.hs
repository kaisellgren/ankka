module State where

import Scene
import Input

data State = State
    { stateWindowWidth     :: !Int
    , stateWindowHeight    :: !Int
    , frameNumber          :: !Int
    , prevTime             :: !Double
    , scene                :: !Scene
    , input                :: !Input
    }
