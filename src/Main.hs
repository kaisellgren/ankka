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
import Terrain

import Control.Concurrent.STM
import Control.Monad             (unless, void, when)
import Control.Monad.RWS.Strict  (RWST, asks, evalRWST, get, liftIO, put, modify)
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

    texMetal <- makeTexture "texture/metal.jpg"
    texDirt <- makeTexture "texture/dirt.jpg"

    let zDistClosest  = 10
        zDistFarthest = zDistClosest + 20
        zDist         = zDistClosest + ((zDistFarthest - zDistClosest) / 2)
        entity        = Entity
          { angle = 0
          , position = (0, 0) :: Vector2
          , velocity = (5, 5) :: Vector2
          , model = Model
            { points = [(-60, -40), (60, -40), (0, 60)] :: [Vector2]
            , Model.texture = texMetal
            }
          }
        entity2 = Entity
          { angle = 0
          , position = (-2, -5) :: Vector2
          , velocity = (10, 12) :: Vector2
          , model = Model
            { points = [(0, 0), (50, 50), (25, 60)] :: [Vector2]
            , Model.texture = texMetal
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
              { entities = [entity, entity2]
              , width = 5120
              , height = 5120
              , World.texture = texDirt
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
        GL.texture    GL.Texture2D GL.$= GL.Enabled
        GL.normalize  GL.$= GL.Enabled
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
    now <- liftIO GLFW.getTime
    let deltaTime = realToFrac $ fromMaybe 0 now - prevTime state
    processEvents
    put $ state
      { scene = (scene state)
        { world = (world $ scene state)
          { entities = fmap (updateEntity deltaTime) (entities $ world $ scene state)
          }
        }
      }

processEvents :: GameState
processEvents = do
    tc <- asks envEventsChan
    me <- liftIO $ atomically $ tryReadTQueue tc
    case me of
        Just e -> do
            processEvent e
            processEvents
        Nothing -> return ()

processEvent :: Event -> GameState
processEvent e = case e of
    (EventKey win k _ ks _) ->
        when (ks == GLFW.KeyState'Pressed) $ do
            -- Q, Esc: exit
            when (k == GLFW.Key'Q || k == GLFW.Key'Escape) $
              liftIO $ GLFW.setWindowShouldClose win True
            when (k == GLFW.Key'Up) $
              liftIO $ putStrLn "Pressed UP!"

    (EventFramebufferSize _ width height) -> do
        modify $ \s -> s
          { stateWindowWidth  = width
          , stateWindowHeight = height
          }
        adjustWindow

    _ -> return ()

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
        renderTerrain $ world $ scene state
        mapM_ renderEntity $ entities $ world $ scene state
