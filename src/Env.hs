module Env where

import Event

import Control.Concurrent.STM
import qualified Graphics.UI.GLFW as GLFW

data Env = Env
    { envEventsChan    :: TQueue Event
    , envWindow        :: !GLFW.Window
    }
