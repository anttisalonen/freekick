module Freekick.Libsoccer.Formation
where

import Freekick.Libsoccer.Scenario
import Libaddutil.Primitives

data Formation = 
    Formation { formationname   :: String,
                areas           :: [Area],        -- player areas
                scenarios       :: [Scenario]
              }
    deriving (Show, Read, Eq)

formation442 :: Formation
formation442 = Formation "4-4-2" start442 []

start442 :: [Area]
start442 = [p20 (50, 0, 0), 
            p20 (20, 0, 30), p20 (40, 0, 30), p20 (60, 0, 30), p20 (80, 0, 30),
            p20 (20, 0, 60), p20 (40, 0, 60), p20 (60, 0, 60), p20 (80, 0, 60),
            p20 (40, 0, 80), p20 (60, 0, 80)]

p20 :: Point -> Area
p20 p = areaAroundPoint 20 p

p40 :: Point -> Area
p40 p = areaAroundPoint 40 p

areaAroundPoint :: Float -> Point -> Area
areaAroundPoint n (x, _, z) = ((x - d, 0, z - d), (x + d, 0, z + d))
    where d = n / 2
