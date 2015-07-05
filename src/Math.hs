module Math where

import qualified Graphics.Rendering.OpenGL as GL

type Vector2 = (Float, Float)

fromAngle :: Float -> Vector2
fromAngle a = (cos a, sin a) :: Vector2

vadd :: Vector2 -> Vector2 -> Vector2
vadd (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

vsub :: Vector2 -> Vector2 -> Vector2
vsub (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)

vneg :: Vector2 -> Vector2
vneg (x, y) = (-x, -y)

vlength :: Vector2 -> Float
vlength (x, y) = sqrt (x*x + y*y)

vscale :: Vector2 -> Float -> Vector2
vscale (x, y) s = (x * s, y * s)

vunit :: Vector2 -> Vector2
vunit v = vscale v $ (1 / vlength v)

vdot :: Vector2 -> Vector2 -> Float
vdot (x1, y1) (x2, y2) = x1 * x2 + y1 * y2

vscalar :: Vector2 -> Vector2 -> Float
vscalar (x1, y1) (x2, y2) = (x1 * y2) - (x2 * y1)

vector3 :: Vector2 -> GL.Vector3 GL.GLfloat
vector3 (x, y) = GL.Vector3 (realToFrac x) (realToFrac y) 0

vertex2 :: Vector2 -> GL.Vertex2 GL.GLfloat
vertex2 (x, y) = GL.Vertex2 (realToFrac x) (realToFrac y)
