module Libaddutil.Vector
where

type Vector3 = (Float, Float, Float)
type Vector2 = (Float, Float)

nullVector2 :: Vector2
nullVector2 = (0, 0)

nullVector3 :: Vector3
nullVector3 = (0, 0, 0)

addVector2 :: Num a => (a, a) -> (a, a) -> (a, a)
addVector2 (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

scaleVector2 :: Num a => (a, a) -> a -> (a, a)
scaleVector2 (x, y) s = (s * x, s * y)

addVector3 :: Num a => (a, a, a) -> (a, a, a) -> (a, a, a)
addVector3 (x1, y1, z1) (x2, y2, z2) = (x1 + x2, y1 + y2, z1 + z2)

diffVector3 :: Num a => (a, a, a) -> (a, a, a) -> (a, a, a)
diffVector3 (x1, y1, z1) (x2, y2, z2) = (x1 - x2, y1 - y2, z1 - z2)

scaleVector3 :: Num a => (a, a, a) -> a -> (a, a, a)
scaleVector3 (x, y, z) s = (s * x, s * y, s * z)

vectorLength :: Vector3 -> Float
vectorLength (x, y, z) = sqrt $ x*x+y*y+z*z

normalizeVector :: Vector3 -> Vector3
normalizeVector (x, y, z) | x == 0 && y == 0 && z == 0 = nullVector3
                          | otherwise                  = (nx, ny, nz)
    where nx = x / vectorLength (x, y, z)
          ny = y / vectorLength (x, y, z)
          nz = z / vectorLength (x, y, z)

setVector3Length :: Vector3 -> Float -> Vector3
setVector3Length v d = scaleVector3 (normalizeVector v) d

clampVector :: Float -> Float -> Vector3 -> Vector3
clampVector min_ max_ n = if vectorLength n < min_ 
                              then (normalizeVector n) `scaleVector3` min_ 
                              else if vectorLength n > max_
                                       then (normalizeVector n) `scaleVector3` max_
                                       else n

getX :: Vector3 -> Float
getX (x, _, _) = x

getY :: Vector3 -> Float
getY (_, y, _) = y

getZ :: Vector3 -> Float
getZ (_, _, z) = z

pointLineDistance2D :: Vector3 -- ^ From
                  -> Vector3 -- ^ To
                  -> Vector3 -- ^ Point
                  -> Float
pointLineDistance2D (x1,_,z1) (x2,_,z2) (x0,_,z0) = abs (dividend) / sqrt (divisor)
    where dividend = (x2 - x1) * (z1 - z0) - (x1 - x0) * (z2 - z1)
          divisor  = (x2 - x1)^(2 :: Integer) + (z2 - z1)^(2 :: Integer)

radiusFromVectorXZ :: Vector3 -- ^Origin
                 -> Float   -- ^Radius
                 -> Float   -- ^Angle in radians
                 -> Vector3
radiusFromVectorXZ (x,y,z) r a = (x + r * cos a, y, z + r * sin a)

angleFromToXZ :: Vector3    -- ^from
              -> Vector3    -- ^to
              -> Float      -- ^angle in radians [-pi, pi]
angleFromToXZ (x1,_,z1) (x2,_,z2) = atan2 (z2 - z1) (x2 - x1)

middlePoint :: Vector3 -> Vector3 -> Vector3
middlePoint (x1,y1,z1) (x2,y2,z2) = ((x1+x2)/2,(y1+y2)/2,(z1+z2)/2)

moveTo :: Float -> Vector3 -> Vector3 -> Vector3
moveTo p v1@(x1,y1,z1) v2@(x2,y2,z2) = (x1+dx*p,y1+dy*p,z1+dz*p)
    where (dx,dy,dz) = diffVector3 v2 v1
