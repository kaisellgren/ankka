module Engine.Graphics where

import Util.Vector2
import Util.GL

import Control.Applicative

import qualified Graphics.GLUtil as GLU
import qualified Graphics.Rendering.OpenGL as GL

renderVertex :: Vector2 -> IO ()
renderVertex (Vector2 x y) = GL.vertex $ GL.Vertex2 (float2gl x) (float2gl y)

texCoord :: GL.GLfloat -> GL.GLfloat -> IO ()
texCoord u v = GL.texCoord (GL.TexCoord2 u v :: GL.TexCoord2 GL.GLfloat)

renderTriangle :: [Vector2] -> IO ()
renderTriangle vertices =
    GL.renderPrimitive GL.Triangles $ mapM_ (GL.vertex . vertex2) vertices

renderRectangle :: [Vector2] -> IO ()
renderRectangle vertices = GL.renderPrimitive GL.Quads $ mapM_ makeQuadPart $ zip vertices texCoords
  where texCoords = [(1, 1), (0, 1), (0, 0), (1, 0)]
        makeQuadPart (vertex, (a, b)) = do
          renderVertex vertex
          texCoord a b

vector3 :: Vector2 -> GL.Vector3 GL.GLfloat
vector3 (Vector2 x y) = GL.Vector3 (realToFrac x) (realToFrac y) 0

vertex2 :: Vector2 -> GL.Vertex2 GL.GLfloat
vertex2 (Vector2 x y) = GL.Vertex2 (realToFrac x) (realToFrac y)

makeTexture :: FilePath -> IO GL.TextureObject
makeTexture f = do
    t <- either error id <$> GLU.readTexture f
    GL.textureFilter GL.Texture2D GL.$= ((GL.Linear', Nothing), GL.Linear')
    GLU.texture2DWrap GL.$= (GL.Mirrored, GL.ClampToEdge)
    pure t
