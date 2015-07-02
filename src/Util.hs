module Util where

import qualified Graphics.UI.GLFW as GLFW
import Control.Monad
import Data.Maybe

withWindow :: Int -> Int -> String -> (GLFW.Window -> IO ()) -> IO ()
withWindow width height title fn = do
    handleErrors
    result <- GLFW.init
    when result $ do
        w <- GLFW.createWindow width height title Nothing Nothing
        case w of
          (Just win) -> do
              GLFW.makeContextCurrent w
              fn win
               -- The game overwrote the error handling.
               -- Let's set it back now that the game has ended.
              handleErrors
              GLFW.destroyWindow win
          Nothing -> return ()
        GLFW.terminate
  where handleErrors = GLFW.setErrorCallback $ Just simpleErrorCallback

simpleErrorCallback :: GLFW.Error -> String -> IO ()
simpleErrorCallback e s = putStrLn $ unwords [show e, show s]

getCursorKeyDirections :: GLFW.Window -> IO (Double, Double)
getCursorKeyDirections win = do
    x0 <- isPress `fmap` GLFW.getKey win GLFW.Key'Up
    x1 <- isPress `fmap` GLFW.getKey win GLFW.Key'Down
    y0 <- isPress `fmap` GLFW.getKey win GLFW.Key'Left
    y1 <- isPress `fmap` GLFW.getKey win GLFW.Key'Right
    let x0n = if x0 then (-1) else 0
        x1n = if x1 then   1  else 0
        y0n = if y0 then (-1) else 0
        y1n = if y1 then   1  else 0
    return (x0n + x1n, y0n + y1n)

isPress :: GLFW.KeyState -> Bool
isPress GLFW.KeyState'Pressed   = True
isPress GLFW.KeyState'Repeating = True
isPress _                       = False

showModifierKeys :: GLFW.ModifierKeys -> String
showModifierKeys mk =
    "[mod keys: " ++ keys ++ "]"
  where
    keys = if null xs then "none" else unwords xs
    xs = catMaybes ys
    ys = [ if GLFW.modifierKeysShift   mk then Just "shift"   else Nothing
         , if GLFW.modifierKeysControl mk then Just "control" else Nothing
         , if GLFW.modifierKeysAlt     mk then Just "alt"     else Nothing
         , if GLFW.modifierKeysSuper   mk then Just "super"   else Nothing
         ]

curb :: Ord a => a -> a -> a -> a
curb l h x
  | x < l     = l
  | x > h     = h
  | otherwise = x
