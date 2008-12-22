module Libaddutil.Collision
where

import Libaddutil.Primitives

boxIntersect :: Area -> Area -> Bool
boxIntersect ((x1,y1,z1),(x2,y2,z2)) ((x3,y3,z3),(x4,y4,z4)) = 
    if x2 < x3 || y2 < y3 || z2 < z3 || 
       x1 > x4 || y1 > y4 || z1 > z4 then False else True

{-                  ((x1 > x3 && x1 < x4) || (x2 > x3 && x2 < x4) &&
                   (y1 > y3 && y1 < y4) || (y2 > y3 && y2 < y4) &&
                   (z1 < z3 && 41 < z4) || (z2 > z3 && z2 < z4))
-}
