module Main where

import Util
import Event
import Callbacks

import Control.Concurrent.STM
import Control.Monad             (unless, void)
import Control.Monad.RWS.Strict  (RWST, asks, evalRWST, get, liftIO)

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
    }

type S = RWST Env () State IO

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
          }

    void $ evalRWST (adjustWindow >> run) env state

-- RWST Env () State IO
run :: S ()
run = do
    win <- asks envWindow

    draw
    liftIO $ do
        GLFW.swapBuffers win
        GL.flush
        GLFW.pollEvents

    quit <- liftIO $ GLFW.windowShouldClose win
    unless quit run

adjustWindow :: S ()
adjustWindow = do
    state <- get
    let width  = stateWindowWidth  state
        height = stateWindowHeight state
        zDist  = stateZDist        state

    let pos   = GL.Position 0 0
        size  = GL.Size (fromIntegral width) (fromIntegral height)
        h     = fromIntegral height / fromIntegral width :: Double
        znear = 1           :: Double
        zfar  = 40          :: Double
        xmax  = znear * 0.5 :: Double
    liftIO $ do
        GL.viewport   GL.$= (pos, size)
        GL.matrixMode GL.$= GL.Projection
        GL.loadIdentity
        GL.frustum (realToFrac $ -xmax)
                   (realToFrac    xmax)
                   (realToFrac $ -xmax * realToFrac h)
                   (realToFrac $  xmax * realToFrac h)
                   (realToFrac    znear)
                   (realToFrac    zfar)
        GL.matrixMode GL.$= GL.Modelview 0
        GL.loadIdentity
        GL.translate (GL.Vector3 0 0 (negate $ realToFrac zDist) :: GL.Vector3 GL.GLfloat)

draw :: S ()
draw = do
    state <- get
    let xa = stateXAngle state
        ya = stateYAngle state
        za = stateZAngle state
        color = GL.Color3 1 0 0 :: GL.Color3 GL.GLfloat
        points = [(-0.6, -0.4), (0.6, -0.4), (0, 0.6)]
    liftIO $ do
        GL.clear [GL.ColorBuffer, GL.DepthBuffer]
        GL.color color
        GL.renderPrimitive GL.Triangles $ do
          GL.vertex $ vertex $ points !! 0
          GL.vertex $ vertex $ points !! 1
          GL.vertex $ vertex $ points !! 2
        where
          vertex (x, y) = GL.Vertex2 x y :: GL.Vertex2 GL.GLfloat
