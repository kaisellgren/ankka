module Main where

import Util
import Event
import Callbacks

import Control.Concurrent.STM
import Control.Monad             (unless, when, void)
import Control.Monad.RWS.Strict  (RWST, ask, asks, evalRWST, get, liftIO, modify, put)

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

    GL.position (GL.Light 0) GL.$= GL.Vertex4 5 5 10 0
    GL.light    (GL.Light 0) GL.$= GL.Enabled
    GL.lighting   GL.$= GL.Enabled
    GL.cullFace   GL.$= Just GL.Back
    GL.depthFunc  GL.$= Just GL.Less
    GL.clearColor GL.$= GL.Color4 0.05 0.05 0.05 1
    GL.normalize  GL.$= GL.Enabled

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
    processEvents

    quit <- liftIO $ GLFW.windowShouldClose win
    unless quit run

-- RWST Env () State IO
processEvents :: S ()
processEvents = do
    tc <- asks envEventsChan
    me <- liftIO $ atomically $ tryReadTQueue tc
    case me of
      Just e -> do
          processEvent e
          processEvents
      Nothing -> return ()

-- RWST Env () State IO
processEvent :: Event -> S ()
processEvent ev =
    case ev of
      (EventError e s) -> do
          printEvent "error" [show e, show s]
          win <- asks envWindow
          liftIO $ GLFW.setWindowShouldClose win True

      (EventWindowPos _ x y) ->
          printEvent "window pos" [show x, show y]

      (EventWindowSize _ width height) ->
          printEvent "window size" [show width, show height]

      (EventWindowClose _) ->
          printEvent "window close" []

      (EventWindowRefresh _) ->
          printEvent "window refresh" []

      (EventWindowFocus _ fs) ->
          printEvent "window focus" [show fs]

      (EventWindowIconify _ is) ->
          printEvent "window iconify" [show is]

      (EventFramebufferSize _ width height) -> do
          printEvent "framebuffer size" [show width, show height]
          modify $ \s -> s
            { stateWindowWidth  = width
            , stateWindowHeight = height
            }
          adjustWindow

      (EventMouseButton _ mb mbs mk) -> do
          printEvent "mouse button" [show mb, show mbs, showModifierKeys mk]
          when (mb == GLFW.MouseButton'1) $ do
              let pressed = mbs == GLFW.MouseButtonState'Pressed
              modify $ \s -> s
                { stateMouseDown = pressed
                }
              unless pressed $
                modify $ \s -> s
                  { stateDragging = False
                  }

      (EventCursorPos _ x y) -> do
          let x' = round x :: Int
              y' = round y :: Int
          printEvent "cursor pos" [show x', show y']
          state <- get
          when (stateMouseDown state && not (stateDragging state)) $
            put $ state
              { stateDragging        = True
              , stateDragStartX      = x
              , stateDragStartY      = y
              , stateDragStartXAngle = stateXAngle state
              , stateDragStartYAngle = stateYAngle state
              }

      (EventCursorEnter _ cs) ->
          printEvent "cursor enter" [show cs]

      (EventScroll _ x y) -> do
          let x' = round x :: Int
              y' = round y :: Int
          printEvent "scroll" [show x', show y']
          env <- ask
          modify $ \s -> s
            { stateZDist =
                let zDist' = stateZDist s + realToFrac (negate $ y / 2)
                in curb (envZDistClosest env) (envZDistFarthest env) zDist'
            }
          adjustWindow

      (EventKey win k scancode ks mk) -> do
          printEvent "key" [show k, show scancode, show ks, showModifierKeys mk]
          when (ks == GLFW.KeyState'Pressed) $ do
              -- Q, Esc: exit
              when (k == GLFW.Key'Q || k == GLFW.Key'Escape) $
                liftIO $ GLFW.setWindowShouldClose win True

      (EventChar _ c) ->
          printEvent "char" [show c]

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
    env   <- ask
    state <- get
    let xa = stateXAngle state
        ya = stateYAngle state
        za = stateZAngle state
        ga = stateGearZAngle  state
    liftIO $ do
        GL.clear [GL.ColorBuffer, GL.DepthBuffer]
        GL.preservingMatrix $ do
            GL.rotate (realToFrac xa) xunit
            GL.rotate (realToFrac ya) yunit
            GL.rotate (realToFrac za) zunit
      where
        xunit = GL.Vector3 1 0 0 :: GL.Vector3 GL.GLfloat
        yunit = GL.Vector3 0 1 0 :: GL.Vector3 GL.GLfloat
        zunit = GL.Vector3 0 0 1 :: GL.Vector3 GL.GLfloat

printEvent :: String -> [String] -> S ()
printEvent cbname fields =
    liftIO $ putStrLn $ cbname ++ ": " ++ unwords fields
