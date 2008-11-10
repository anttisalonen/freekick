module Libaddutil.Misc
where

import Libaddutil.Vector

clamp :: Ord a => a -> a -> a -> a
clamp min_ max_ n = if n < min_ then min_ else if n > max_ then max_ else n

quadr :: (Ord a, Floating a) => a -> a -> a -> [a]
quadr a b c | a == 0 = -c/b:[]
quadr a b c = l1 ++ l2
    where det = b**2 - 4 * a * c
          l1  = if det >= 0 then [(-b + (sqrt det)) / (2 * a)] else []
          l2  = if det >  0 then [(-b - (sqrt det)) / (2 * a)] else []

lineLineIntersection :: Vector3     -- ^(x1,y1) line1
                     -> Vector3     -- ^(x2,y2) line1
                     -> Vector3     -- ^(x3,y3) line2
                     -> Vector3     -- ^(x4,y4) line2
                     -> Vector3     -- ^point of intersection
lineLineIntersection (x1,_,y1) (x2,_,y2) (x3,_,y3) (x4,_,y4) = if xden == 0 && len1 > 0 && len2 > 0 then (x1,0,y1) else (x,0,y)
    where x = xnum / xden
          y = ynum / yden
          xnum = (x1*y2-y1*x2)*(x3-x4)-(x1-x2)*(x3*y4-y3*x4)
          xden = (x1-x2)*(y3-y4)      -(y1-y2)*(x3-x4)
          ynum = (x1*y2-y1*x2)*(y3-y4)-(y1-y2)*(x3*y4-y3*x4)
          yden = xden
          len1 = vectorLength (x2-x1,0,y2-y1)
          len2 = vectorLength (x4-x3,0,y4-y3)

segmentSegmentIntersection :: Vector3 -> Vector3 -> Vector3 -> Vector3 -> Maybe Vector3
segmentSegmentIntersection (x1,_,y1) (x2,_,y2) (x3,_,y3) (x4,_,y4) = 
    if len1 > 0 && len2 > 0 && x >= minx && x <= maxx && y >= miny && y <= maxy then Just (x,0,y) else Nothing
        where minx = x1 `min` x2
              maxx = x1 `max` x2
              miny = y1 `min` y2
              maxy = y1 `max` y2
              (x,_,y) = lineLineIntersection (x1,0,y1) (x2,0,y2) (x3,0,y3) (x4,0,y4)
              len1 = vectorLength (x2-x1,0,y2-y1)
              len2 = vectorLength (x4-x3,0,y4-y3)
