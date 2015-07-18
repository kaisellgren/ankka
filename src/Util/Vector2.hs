module Util.Vector2 where

import Text.Printf

data Vector2 = Vector2 Float Float

fromAngle :: Float -> Vector2
fromAngle a = Vector2 (cos a) (sin a)

vadd :: Vector2 -> Vector2 -> Vector2
vadd (Vector2 x1 y1) (Vector2 x2 y2) = Vector2 (x1 + x2) (y1 + y2)

vsub :: Vector2 -> Vector2 -> Vector2
vsub (Vector2 x1 y1) (Vector2 x2 y2) = Vector2 (x1 - x2) (y1 - y2)

vneg :: Vector2 -> Vector2
vneg (Vector2 x y) = Vector2 (-x) (-y)

vlength :: Vector2 -> Float
vlength (Vector2 x y) = sqrt (x*x + y*y)

vscale :: Vector2 -> Float -> Vector2
vscale (Vector2 x y) s = Vector2 (x * s) (y * s)

vunit :: Vector2 -> Vector2
vunit v = vscale v $ 1 / vlength v

vdot :: Vector2 -> Vector2 -> Float
vdot (Vector2 x1 y1) (Vector2 x2 y2) = x1 * x2 + y1 * y2

vscalar :: Vector2 -> Vector2 -> Float
vscalar (Vector2 x1 y1) (Vector2 x2 y2) = (x1 * y2) - (x2 * y1)

instance PrintfArg Vector2 where
    formatArg (Vector2 x y) = formatArg ("(" ++ show x ++ ", " ++ show y ++ ")")
