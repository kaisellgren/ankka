module Main where

import Util.Util
import Engine.Event
import Engine.Callbacks
import Util.Vector2
import Engine.Graphics
import Game.Bush
import Game.World
import Game.Scene
import Game.Entity
import Game.Model
import Game.Terrain
import Engine.Env
import Engine.State
import qualified Engine.Input as Input

import Control.Applicative
import Control.Concurrent.STM
import Control.Monad             (unless, void, when)
import Control.Monad.RWS.Strict  (RWST, asks, evalRWST, get, liftIO, put, modify)
import Data.Maybe
import Text.Printf
import Graphics.Rendering.FTGL
import System.Random

import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW

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
    texBush01 <- makeTexture "texture/bush01.png"
    texBush02 <- makeTexture "texture/bush02.png"
    texBush03 <- makeTexture "texture/bush03.png"
    texBush04 <- makeTexture "texture/bush04.png"
    texBush05 <- makeTexture "texture/bush05.png"
    openSans <- createTextureFont "fonts/OpenSans-Regular.ttf"

    setFontFaceSize openSans 24 72

    let bushTextures = [texBush01, texBush02, texBush03, texBush04, texBush05]
    let randomBushTex r = pure $ bushTextures !! floor (r * 5)
    bushTex <- do (randomIO :: IO Float) >>= randomBushTex
    bushTex2 <- do (randomIO :: IO Float) >>= randomBushTex

    let bush = createBush (Vector2 0 0) bushTex
        bush2 = createBush (Vector2 256 256) bushTex2
        env = Env
          { envEventsChan    = eventChannel
          , envWindow        = win
          , envOpenSans      = openSans
          }
        state = State
          { stateWindowWidth     = fbWidth
          , stateWindowHeight    = fbHeight
          , frameNumber          = 0
          , prevTime             = 0
          , scene                = Scene
            { world = World
              { bushEntities = [bush, bush2]
              , width = 5120
              , height = 5120
              , Game.World.texture = texDirt
              }
            , cameraPosition = Vector2 0 0
            }
          , input = Input.Input
            { Input.pressedKeys = Input.pressedKeysDefault }
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
        GL.blend      GL.$= GL.Enabled
        GL.blendFunc  GL.$= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
        GL.loadIdentity

update :: GameState
update = do
    processEvents

    state <- get
    now <- liftIO GLFW.getTime
    let deltaTime = realToFrac $ fromMaybe 0 now - prevTime state

    updateCamera deltaTime

    modify $ \s -> s
      { scene = (scene s)
        { world = (world $ scene s)
          { bushEntities = fmap (updateBush deltaTime) (bushEntities $ world $ scene s)
          }
        }
      }

updateCamera :: Float -> GameState
updateCamera deltaTime = do
    state <- get
    when (Input.up $ Input.pressedKeys $ input state) $ moveCamera 0 1000
    when (Input.down $ Input.pressedKeys $ input state) $ moveCamera 0 (-1000)
    when (Input.right $ Input.pressedKeys $ input state) $ moveCamera 1000 0
    when (Input.left $ Input.pressedKeys $ input state) $ moveCamera (-1000) 0
  where moveCamera x y = modify $ \s -> s
          { scene = (scene s)
            { cameraPosition = vadd (cameraPosition (scene s)) $ Vector2 (x * deltaTime) (y * deltaTime)
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
        Nothing -> pure ()

processEvent :: Event -> GameState
processEvent e = case e of
    (EventKey win k _ ks _) -> do
        let isKeyDown = ks == GLFW.KeyState'Pressed
            isKeyUp = ks == GLFW.KeyState'Released

        when (isKeyDown || isKeyUp) $ do
            when (k == GLFW.Key'Up) $ toggleKey $ \state -> (Input.pressedKeys $ input state) { Input.up = isKeyDown }
            when (k == GLFW.Key'Right) $ toggleKey $ \state -> (Input.pressedKeys $ input state) { Input.right = isKeyDown }
            when (k == GLFW.Key'Down) $ toggleKey $ \state -> (Input.pressedKeys $ input state) { Input.down = isKeyDown }
            when (k == GLFW.Key'Left) $ toggleKey $ \state -> (Input.pressedKeys $ input state) { Input.left = isKeyDown }

        when isKeyDown $ do
            when (k == GLFW.Key'Q || k == GLFW.Key'Escape) $ liftIO $ GLFW.setWindowShouldClose win True

      where toggleKey fn = modify $ \state -> state
              { input = (input state)
                { Input.pressedKeys = fn state
                }
              }

    (EventFramebufferSize _ width height) -> do
        modify $ \s -> s
          { stateWindowWidth  = width
          , stateWindowHeight = height
          }
        adjustWindow

    _ -> pure ()

draw :: GameState
draw = do
    win <- asks envWindow
    openSans <- asks envOpenSans
    state <- get
    let w = stateWindowWidth state
        h = stateWindowHeight state
    let color = GL.Color3 1 0 0 :: GL.Color3 GL.GLfloat
        pTime = prevTime state
        camPos = cameraPosition $ scene state
    now <- liftIO GLFW.getTime
    let deltaTime = fromMaybe 0 now - pTime
    put $ state
      { frameNumber = frameNumber state + 1
      , prevTime    = fromMaybe 0 now
      }
    liftIO $ do
        GL.clear [GL.ColorBuffer, GL.DepthBuffer]
        GL.loadIdentity
        GL.translate $ vector3 $ vneg camPos
        renderTerrain $ world $ scene state
        mapM_ (renderEntity . entity) $ bushEntities $ world $ scene state

        GL.loadIdentity
        GL.translate $ vector3 $ Vector2 (fromIntegral (-w) / 2.0) (fromIntegral h / 2.0 - 24)
        renderFont openSans (printf "frame: %d, deltaTime: %.3f, camPos: %s" (frameNumber state) deltaTime camPos) All
