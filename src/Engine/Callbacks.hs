module Engine.Callbacks where

import Engine.Event

import Control.Concurrent.STM

import qualified Graphics.UI.GLFW as GLFW

errorCallback           :: TQueue Event -> GLFW.Error -> String -> IO ()
windowPosCallback       :: TQueue Event -> GLFW.Window -> Int -> Int -> IO ()
windowSizeCallback      :: TQueue Event -> GLFW.Window -> Int -> Int -> IO ()
windowCloseCallback     :: TQueue Event -> GLFW.Window -> IO ()
windowRefreshCallback   :: TQueue Event -> GLFW.Window -> IO ()
windowFocusCallback     :: TQueue Event -> GLFW.Window -> GLFW.FocusState -> IO ()
windowIconifyCallback   :: TQueue Event -> GLFW.Window -> GLFW.IconifyState -> IO ()
framebufferSizeCallback :: TQueue Event -> GLFW.Window -> Int -> Int -> IO ()
mouseButtonCallback     :: TQueue Event -> GLFW.Window -> GLFW.MouseButton -> GLFW.MouseButtonState -> GLFW.ModifierKeys -> IO ()
cursorPosCallback       :: TQueue Event -> GLFW.Window -> Double -> Double -> IO ()
cursorEnterCallback     :: TQueue Event -> GLFW.Window -> GLFW.CursorState -> IO ()
scrollCallback          :: TQueue Event -> GLFW.Window -> Double -> Double -> IO ()
keyCallback             :: TQueue Event -> GLFW.Window -> GLFW.Key -> Int -> GLFW.KeyState -> GLFW.ModifierKeys -> IO ()
charCallback            :: TQueue Event -> GLFW.Window -> Char -> IO ()

errorCallback           tc e s            = atomically $ writeTQueue tc $ EventError           e s
windowPosCallback       tc win x y        = atomically $ writeTQueue tc $ EventWindowPos       win x y
windowSizeCallback      tc win w h        = atomically $ writeTQueue tc $ EventWindowSize      win w h
windowCloseCallback     tc win            = atomically $ writeTQueue tc $ EventWindowClose     win
windowRefreshCallback   tc win            = atomically $ writeTQueue tc $ EventWindowRefresh   win
windowFocusCallback     tc win fa         = atomically $ writeTQueue tc $ EventWindowFocus     win fa
windowIconifyCallback   tc win ia         = atomically $ writeTQueue tc $ EventWindowIconify   win ia
framebufferSizeCallback tc win w h        = atomically $ writeTQueue tc $ EventFramebufferSize win w h
mouseButtonCallback     tc win mb mba mk  = atomically $ writeTQueue tc $ EventMouseButton     win mb mba mk
cursorPosCallback       tc win x y        = atomically $ writeTQueue tc $ EventCursorPos       win x y
cursorEnterCallback     tc win ca         = atomically $ writeTQueue tc $ EventCursorEnter     win ca
scrollCallback          tc win x y        = atomically $ writeTQueue tc $ EventScroll          win x y
keyCallback             tc win k sc ka mk = atomically $ writeTQueue tc $ EventKey             win k sc ka mk
charCallback            tc win c          = atomically $ writeTQueue tc $ EventChar            win c

setCallbacks :: TQueue Event -> GLFW.Window -> IO ()
setCallbacks eventChannel win = do
    GLFW.setErrorCallback           $ Just $ errorCallback           eventChannel
    GLFW.setWindowPosCallback       win $ Just $ windowPosCallback       eventChannel
    GLFW.setWindowSizeCallback      win $ Just $ windowSizeCallback      eventChannel
    GLFW.setWindowCloseCallback     win $ Just $ windowCloseCallback     eventChannel
    GLFW.setWindowRefreshCallback   win $ Just $ windowRefreshCallback   eventChannel
    GLFW.setWindowFocusCallback     win $ Just $ windowFocusCallback     eventChannel
    GLFW.setWindowIconifyCallback   win $ Just $ windowIconifyCallback   eventChannel
    GLFW.setFramebufferSizeCallback win $ Just $ framebufferSizeCallback eventChannel
    GLFW.setMouseButtonCallback     win $ Just $ mouseButtonCallback     eventChannel
    GLFW.setCursorPosCallback       win $ Just $ cursorPosCallback       eventChannel
    GLFW.setCursorEnterCallback     win $ Just $ cursorEnterCallback     eventChannel
    GLFW.setScrollCallback          win $ Just $ scrollCallback          eventChannel
    GLFW.setKeyCallback             win $ Just $ keyCallback             eventChannel
    GLFW.setCharCallback            win $ Just $ charCallback            eventChannel
