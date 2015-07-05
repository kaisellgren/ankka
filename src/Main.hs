module Main where

import Util
import Event
import Callbacks
import Math
import Graphics
import World
import Scene
import Entity
import Model

import Control.Concurrent.STM
import Control.Monad             (unless, void)
import Control.Monad.RWS.Strict  (RWST, asks, evalRWST, get, liftIO, put)
import Data.Maybe
import Text.Printf

import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW

data Env = Env
    { envEventsChan    :: TQueue Event
    , envWindow        :: !GLFW.Window
    , envZDistClosest  :: !Double
    , envZDistFarthest :: !Double
    }

data State = State
    { stateWindowWidth     :: !Int
    , stateWindowHeight    :: !Int
    , stateXAngle          :: !Double
    , stateYAngle          :: !Double
    , stateZAngle          :: !Double
    , stateGearZAngle      :: !Double
    , stateZDist           :: !Double
    , stateMouseDown       :: !Bool
    , stateDragging        :: !Bool
    , stateDragStartX      :: !Double
    , stateDragStartY      :: !Double
    , stateDragStartXAngle :: !Double
    , stateDragStartYAngle :: !Double
    , frameNumber          :: !Int
    , prevTime             :: !Double
    , scene                :: !Scene
    }

type GameState = RWST Env () State IO ()

main :: IO ()
main = do
    eventChannel <- newTQueueIO :: IO (TQueue Event)
    withWindow 800 600 "Ankka" $ \win -> do
        setCallbacks eventChannel win
        initialize eventChannel win

initialize :: TQueue Event -> GLFW.Window -> IO ()
initialize eventChannel win = do
    GLFW.swapInterval 1

    GL.clearColor GL.$= GL.Color4 0.05 0.05 0.05 1

    (fbWidth, fbHeight) <- GLFW.getFramebufferSize win

    let zDistClosest  = 10
        zDistFarthest = zDistClosest + 20
        zDist         = zDistClosest + ((zDistFarthest - zDistClosest) / 2)
        entity        = Entity
          { angle = 0
          , position = (0, 0) :: Vector2
          , velocity = (5, 5) :: Vector2
          , model = Model
            { points = [(-60, -40), (60, -40), (0, 60)] :: [Vector2]
            }
          }
        env = Env
          { envEventsChan    = eventChannel
          , envWindow        = win
          , envZDistClosest  = zDistClosest
          , envZDistFarthest = zDistFarthest
          }
        state = State
          { stateWindowWidth     = fbWidth
          , stateWindowHeight    = fbHeight
          , stateXAngle          = 0
          , stateYAngle          = 0
          , stateZAngle          = 0
          , stateGearZAngle      = 0
          , stateZDist           = zDist
          , stateMouseDown       = False
          , stateDragging        = False
          , stateDragStartX      = 0
          , stateDragStartY      = 0
          , stateDragStartXAngle = 0
          , stateDragStartYAngle = 0
          , frameNumber          = 0
          , prevTime             = 0
          , scene                = Scene
            { world = World
              { entities = [entity]
              }
            , cameraPosition = (0, 0) :: Vector2
            }
          }

    void $ evalRWST (adjustWindow >> run) env state

run :: GameState
run = do
    win <- asks envWindow

    update
    draw

    liftIO $ do
        GLFW.swapBuffers win
        GL.flush
        GLFW.pollEvents

    quit <- liftIO $ GLFW.windowShouldClose win
    unless quit run

adjustWindow :: GameState
adjustWindow = do
    state <- get
    let width  = stateWindowWidth  state
        height = stateWindowHeight state

    let pos   = GL.Position 0 0
        size  = GL.Size (fromIntegral width) (fromIntegral height)
    liftIO $ do
        GL.viewport   GL.$= (pos, size)
        GL.matrixMode GL.$= GL.Projection
        GL.loadIdentity
        GL.ortho2D (-fromIntegral width / 2)
                   (fromIntegral width / 2)
                   (-fromIntegral height / 2)
                   (fromIntegral height / 2)
        GL.matrixMode GL.$= GL.Modelview 0
        GL.loadIdentity

update :: GameState
update = do
    state <- get
    put $ state
      { scene = (scene state)
        { world = (world $ scene state)
          { entities = fmap updateEntity [head $ entities $ world $ scene state]
          }
        }
      }

draw :: GameState
draw = do
    win <- asks envWindow
    state <- get
    let color = GL.Color3 1 0 0 :: GL.Color3 GL.GLfloat
        pTime = prevTime state
    now <- liftIO GLFW.getTime
    let deltaTime = fromMaybe 0 now - pTime
    put $ state
      { frameNumber = frameNumber state + 1
      , prevTime    = fromMaybe 0 now
      }
    liftIO $ GLFW.setWindowTitle win $ printf "frame: %d, deltaTime: %.3f" (frameNumber state) deltaTime
    liftIO $ do
        GL.clear [GL.ColorBuffer, GL.DepthBuffer]
        GL.color color
        renderEntity (head $ entities $ world $ scene state :: Entity)
