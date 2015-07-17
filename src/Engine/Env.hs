module Engine.Env where

import Engine.Event

import Control.Concurrent.STM
import Graphics.Rendering.FTGL
import qualified Graphics.UI.GLFW as GLFW

data Env = Env
    { envEventsChan    :: TQueue Event
    , envWindow        :: !GLFW.Window
    , envOpenSans         :: !Font
    }
