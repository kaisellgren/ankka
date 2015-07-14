module Terrain where

import World
import Graphics

import qualified Graphics.Rendering.OpenGL as GL

renderTerrain :: World -> IO ()
renderTerrain world = do
    let size = 512
        nHor = quot (width world) size
        nVer = quot (height world) size
    GL.activeTexture GL.$= GL.TextureUnit 0
    GL.textureBinding GL.Texture2D GL.$= Just (texture world)
    GL.renderPrimitive GL.Quads $ mapM_ (makeQuad size) [(x, y) | x <- [0..nHor - 1], y <- [0..nVer - 1]]

  where
        makeQuad size (x, y) = GL.renderPrimitive GL.Quads $ do
            let offsetX = size * x
                offsetY = size * y
            renderVertex (offsetX + size) (offsetY + size)
            texCoord 1 1
            renderVertex (offsetX + size) offsetY
            texCoord 0 1
            renderVertex offsetX offsetY
            texCoord 0 0
            renderVertex offsetX (offsetY + size)
            texCoord 1 0
