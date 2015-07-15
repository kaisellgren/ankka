module State where

import Scene

data State = State
    { stateWindowWidth     :: !Int
    , stateWindowHeight    :: !Int
    , frameNumber          :: !Int
    , prevTime             :: !Double
    , scene                :: !Scene
    }
