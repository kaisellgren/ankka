module Graphics where

import Math
import Entity
import Model

import Control.Applicative

import qualified Graphics.GLUtil as GLU
import qualified Graphics.Rendering.OpenGL as GL

renderTriangle :: [Vector2] -> IO ()
renderTriangle vertices =
    GL.renderPrimitive GL.Triangles $ mapM_ (GL.vertex . vertex2) vertices

renderEntity :: Entity -> IO ()
renderEntity e = renderTriangle $ fmap (vadd $ position e) (points (model e))

vector3 :: Vector2 -> GL.Vector3 GL.GLfloat
vector3 (x, y) = GL.Vector3 (realToFrac x) (realToFrac y) 0

vertex2 :: Vector2 -> GL.Vertex2 GL.GLfloat
vertex2 (x, y) = GL.Vertex2 (realToFrac x) (realToFrac y)

makeTexture :: FilePath -> IO GL.TextureObject
makeTexture f = do
    t <- either error id <$> GLU.readTexture f
    GL.textureFilter GL.Texture2D GL.$= ((GL.Linear', Nothing), GL.Linear')
    GLU.texture2DWrap GL.$= (GL.Mirrored, GL.ClampToEdge)
    return t
