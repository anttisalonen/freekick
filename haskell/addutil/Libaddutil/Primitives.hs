module Libaddutil.Primitives
where

import System.Time
import Libaddutil.Colors
import Libaddutil.Vector

type Point = Vector3

type Area  = (Point, Point)

areaMiddlePoint :: Area -> Point
areaMiddlePoint ((x1,y1,z1),(x2,y2,z2)) = ((x2 - x1) / 2 + x1, (y2 - y1) / 2 + y1, (z2 - z1) / 2 + z1)

nullPoint :: Point
nullPoint = (0,0,0)

nullArea :: Area
nullArea = (nullPoint, nullPoint)

pointInArea2D :: Area -> Point -> Bool
pointInArea2D ((x1,_,z1),(x2,_,z2)) (x0,_,z0) = x0 >= x1 && x0 <= x2 && z0 >= z1 && z0 <= z2

inBox :: Vector3 -- ^top left
      -> Vector3 -- ^bottom right
      -> Vector3 -- ^to check for
      -> Bool
inBox (x1,_,z1) (x2,_,z2) (x,_,z) = x > x1 && x < x2 && z > z1 && z < z2

inArea :: Area -> Vector3 -> Bool
inArea (a, b) n = inBox a b n

inCircle :: Vector3 -> Vector3 -> Float -> Bool
inCircle (x,_,z) (x1,_,z1) r = vectorLength (diffVector3 (x,0,z) (x1,0,z1)) < r

type PointLight = (Point, Color)

type Date = (Month, Int)

oneMonthLess :: Month -> Month
oneMonthLess January        = December
oneMonthLess February       = January
oneMonthLess March          = February
oneMonthLess April          = March
oneMonthLess May            = April
oneMonthLess June           = May
oneMonthLess July           = June
oneMonthLess August         = July
oneMonthLess September      = August
oneMonthLess October        = September
oneMonthLess November       = October
oneMonthLess December       = November

oneMonthMore :: Month -> Month
oneMonthMore January        = February
oneMonthMore February       = March
oneMonthMore March          = April
oneMonthMore April          = May
oneMonthMore May            = June
oneMonthMore June           = July
oneMonthMore July           = August
oneMonthMore August         = September
oneMonthMore September      = October
oneMonthMore October        = November
oneMonthMore November       = December
oneMonthMore December       = January

diffDateInDays :: Date -> Date -> Int
diffDateInDays (em, ed) (sm, sd) = if em < sm || (em == sm && ed < sd) 
                                       then diffDateInDays (December, 31) (sm, sd) + diffDateInDays (em, ed) (January, 1)
                                       else if em == sm 
                                                then (ed - sd)
                                                else ed + diffDateInDays (oneMonthLess em, daysInMonth (oneMonthLess em)) (sm, sd)

earlierDate :: Date -> Date -> Bool
earlierDate (m1, d1) (m2, d2) = if m1 < m2 || (m1 == m2 && d1 < d2) then True else False

addDaysToDate :: Date -> Int -> Date
addDaysToDate (m, d) a = if (d + a) <= daysInMonth m then (m, (d + a)) else ((oneMonthMore m), (d + a) - daysInMonth m)

daysInMonth :: Month -> Int
daysInMonth January    = 31
daysInMonth February   = 28
daysInMonth March      = 31
daysInMonth April      = 30
daysInMonth May        = 31
daysInMonth June       = 30
daysInMonth July       = 31
daysInMonth August     = 31
daysInMonth September  = 30
daysInMonth October    = 31
daysInMonth November   = 30
daysInMonth December   = 31

type Time = (Int, Int, Int)

type PreciseTime = (Int, Int, Int)

type Schedule = (Date, Date)        -- Start, end

type DateTime = (Date, Time)

getDate :: DateTime -> Date
getDate d = fst d

nullDate :: Date
nullDate = (January, 1)

nullTime :: Time
nullTime = (0, 0, 0)

nullSchedule :: Schedule
nullSchedule = (nullDate, nullDate)

scheduleLength :: Schedule -> Int
scheduleLength (d1, d2) = d2 `diffDateInDays` d1

beginDate :: Schedule -> Date
beginDate d = fst d

endDate :: Schedule -> Date
endDate d = snd d

showDate :: Date -> String
showDate (m, d) = show m ++ ", " ++ show d

dateEquals :: Date -> DateTime -> Bool
dateEquals r d = fst d == r

addPreciseTime :: PreciseTime -> PreciseTime -> PreciseTime
addPreciseTime (m1,s1,u1) (m2,s2,u2) = (mn,sn,un)
    where un     = (u1 + u2) `mod` 1000
          ucarry = (u1 + u2) `div` 1000
          stot   = (s1 + s2) + ucarry
          sn     = stot `mod` 60
          scarry = stot `div` 60
          mn     = m1 + m2 + scarry

timeDiffToPreciseTime :: TimeDiff -> PreciseTime
timeDiffToPreciseTime d = (mn,sn,un)
    where p  = tdPicosec d
          s  = tdSec d
          un = fromEnum $ p `div` 1000000000
          sn = fromEnum $ s `mod` 60
          mn = fromEnum $ s `div` 60
