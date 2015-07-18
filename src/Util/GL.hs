module Util.GL where

import qualified Graphics.Rendering.OpenGL as GL

float2gl :: Float -> GL.GLfloat
float2gl = realToFrac :: Float -> GL.GLfloat
