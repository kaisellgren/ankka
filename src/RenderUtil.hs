module RenderUtil where

import Math
import qualified Graphics.Rendering.OpenGL as GL

renderTriangle :: [Vector2] -> IO ()
renderTriangle vertices =
    GL.renderPrimitive GL.Triangles $ mapM_ (GL.vertex . vertex2) vertices
      where vertex2 (x, y) = GL.Vertex2 (realToFrac x) (realToFrac y) :: GL.Vertex2 GL.GLfloat
