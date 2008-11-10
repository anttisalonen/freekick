module Helpers
where

import Data.Maybe
import Data.List

import Libaddutil.Vector
import Libaddutil.ListUtils
import Libaddutil.Primitives

import Freekick.Libsoccer.Pitch
import Freekick.Libsoccer.Lineup
import Freekick.Libsoccer.Formation
import Freekick.Libsoccer.Player

import Freekick.Libmatch.PlayerInfo
import Freekick.Libmatch.Rules

import Parameters

-- | A value between 0 and 1 describing the threat the nearby opponents pose,
-- 1 being the least threat. Only the 'maxClosestThreats' nearest opponents are taken
-- into consideration.
opponentsSafeScore :: [Vector3] -> Vector3 -> Float
opponentsSafeScore ps (x,y,z) = let dists = take maxClosestThreats $ sortBy compare (map vectorLength (map (diffVector3 (x,y,z)) ps))
                                    thrts = map distanceToThreat dists
                                in 1 - sum thrts / (toEnum maxClosestThreats)

distanceToThreat :: Float -> Float
distanceToThreat d = if d > maxThreatDistance then 0 else ((maxThreatDistance - d) / maxThreatDistance) ** threatAssumedAggressiveness

nearestThreat :: [Vector3] -> Vector3 -> Vector3
nearestThreat [] _       = (0,0,0)
nearestThreat ps (x,y,z) = fst $ mapPreserveMin (vectorLength . diffVector3 (x, y, z)) ps

-- | Value of a spot, e.g. 1 in front of the opposing goal.
spotValue :: Pitch -> PitchDirection -> Vector3 -> Float
spotValue pit d (x,_,z) = let (px,_,pz) = area pit
                              zrel      = z / pz
                              ogdist    = vectorLength (diffVector3 (getGoal pit (other d)) (x,0,z))
                              agdist    = vectorLength (diffVector3 (getGoal pit  d       ) (x,0,z))
                              pxh       = px / 2
                              agp       = if agdist > pxh then 0 else 0.5 * (pxh - agdist) / pxh
                              ogp       = if ogdist > pxh then 0 else 0.5 * (pxh - ogdist) / pxh
                              xdiff     = 0.5 + agp - ogp
                              zdiff     = if d == Down then zrel else 1 - zrel
                          in if x < 0 || x > px || z < 0 || z > pz then (-1) else ((zdiff + xdiff)/2)**spotValueFactor

opposingGoal :: PlayerInfo -> Pitch -> Vector3
opposingGoal p pit | direction p == Up   = (getMidLine pit, 0, 0)
                   | otherwise           = (getMidLine pit, 0, getBottomLine pit)

getPlayerArea :: Player -> Lineup -> Formation -> Maybe Area
getPlayerArea p l f = findFromPairs (zip (lineupToList l) (areas f)) (idnum p)

formationSpot :: PlayerInfo -> Pitch -> Formation -> Lineup -> Vector3
formationSpot p pit f l = let (fx,_,fz) = kickoffFormationSpot p pit f l
                              (_,_,pz)  = area pit
                          in if direction p == Down then (fx,0,fz*2) else (fx,0,(fz-(pz/2))*2)

kickoffFormationSpot :: PlayerInfo -> Pitch -> Formation -> Lineup -> Vector3
kickoffFormationSpot p pit f l = (x,0,z)
    where (xrel,_,zrel)      = areaMiddlePoint (fromMaybe (substitutesBench pit (ishomeclub p)) (getPlayerArea (staticplayer p) l f))
          (pwidth,_,pheight) = area pit
          pd                 = direction p
          x                  = if pd == Up then pwidth  - (pwidth  * xrel / 100) else pwidth  * xrel / 100
          z                  = if pd == Up then pheight - (pheight * zrel / 200) else pheight * zrel / 200

