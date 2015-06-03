module Main where

import Graphics.UI.GLUT

main :: IO ()
main = do
    (imageName, arguments) <- getArgsAndInitialize
    window <- createWindow "Tsaukki maailma!"
    displayCallback $= display
    mainLoop

display :: DisplayCallback
display = do
    clear [ ColorBuffer ]
    flush
