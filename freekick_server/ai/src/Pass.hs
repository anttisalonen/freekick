module Pass
where

import Data.List
import Data.Maybe

import Libaddutil.Entity
import Libaddutil.Vector
import Libaddutil.ListUtils

import Freekick.Libsoccer.Pitch

import Freekick.Libmatch.PlayerInfo
import Freekick.Libmatch.SoccerPhysics

import Helpers
import Parameters

bestPass :: PlayerInfo          -- ^Player self
         -> [PlayerInfo]        -- ^Team mates
         -> [PlayerInfo]        -- ^Opponents
         -> Pitch
         -> (PlayerInfo, (Float, Vector3)) -- ^(team mate to whom to pass, (value, vector where to pass))
bestPass p ts os pit = mapPreserveMaxMaybes (totalPassScore pit os p) ts

totalPassScore :: Pitch        -- ^Pitch
               -> [PlayerInfo] -- ^Opponents
               -> PlayerInfo   -- ^Source
               -> PlayerInfo   -- ^Target
               -> Maybe (Float, Vector3)
totalPassScore pit os s t = let ols = map (location . plentity) os
                                sl  = (location . plentity) s
--                                tl  = (location . plentity) t
                                mtl = getPassVector maxPassVelocity (plentity t)
                                tl  = fromJust mtl
                                ps  = passPathSafety ols sl tl
                                di  = passDistanceFactor s tl
                                ss  = opponentsSafeScore ols sl
                                sv  = spotValue pit (direction s) tl
                            in if isNothing mtl then Nothing else Just (ps * di * ss * sv, fromJust mtl)

-- | The "probability" that a pass will not be intercepted. Only the closest
-- opponent to the passing line will be taken into account.
passPathSafety :: [Vector3] -- ^ Opponents
               -> Vector3   -- ^ From-vector
               -> Vector3   -- ^ To-vector
               -> Float
passPathSafety os s t = let threats = map distanceToThreat (map (pointLineDistance2D s t) os)
                        in maximum threats

passDistanceFactor :: PlayerInfo -> Vector3 -> Float
passDistanceFactor s t = let d = vectorLength $ diffVector3 (location (plentity s)) t
                         in if d > maxPassDistance then 0 else ((maxPassDistance - d) / maxPassDistance)
