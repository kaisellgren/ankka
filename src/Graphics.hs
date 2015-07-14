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
renderEntity e = do
    GL.activeTexture GL.$= GL.TextureUnit 0
    GL.textureBinding GL.Texture2D GL.$= Just (texture $ model e)
    let
        glSize = GL.TextureSize2D (gsizei $ fst size) (gsizei $ snd size)
    GL.texImage2D Nothing GL.NoProxy 0 GL.RGB8 glSize 0 (GL.PixelData GL.BGR GL.UnsignedByte bPtr)
    renderTriangle $ fmap (vadd $ position e) (points $ model e :: [Vector2])
    GL.renderPrimitive GL.Quads $ do
        n 0 1 0
        t 0 1 >> v 10 (-10) 10
        t 1 1 >> v 10 (-10) (-10)
        t 1 0 >> v (-10) (-10) (-10)
        t 0 0 >> v (-10) (-10) 10
      where v x y z = GL.vertex (GL.Vertex3 x y z :: GL.Vertex3 GL.GLfloat)
            n x y z = GL.normal (GL.Normal3 x y z :: GL.Normal3 GL.GLfloat)
            t u v = GL.texCoord (GL.TexCoord2 u v :: GL.TexCoord2 GL.GLfloat)

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
