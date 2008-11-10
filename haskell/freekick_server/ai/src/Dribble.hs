module Dribble
where

import Debug.Trace
import Data.List

import Libaddutil.Entity
import Libaddutil.Vector

import Freekick.Libsoccer.Pitch

import Freekick.Libmatch.PlayerInfo
import Freekick.Libmatch.Rules

import Helpers
import Parameters

-- | Calculates the dribble value. TODO: this doesn't take opponents that are
-- very close to the player himself into account.
bestDribbleValue :: PlayerInfo         -- ^player
                 -> [PlayerInfo]       -- ^opponents
                 -> Pitch
                 -> (Vector3, Float)   -- ^(dribble target point, dribble value)
bestDribbleValue p os pit = let tl = nearestThreat (map (location . plentity) os) pl
                                pl = location $ plentity p
                                r  = dribbleDistance
                                oa = angleFromToXZ pl tl
                                ga = angleFromToXZ pl (opposingGoal p pit)
                                ad = ga - oa
                                aa = pi/2 - ad
                                a  = if abs ad > pi/2 then ga else if ad > 0 then ga + aa else ga - aa
                                pt = radiusFromVectorXZ pl r a
                            in (pt, dribbleValueOnPoint pit (direction p) pt tl)

dribbleValueOnPoint :: Pitch
                    -> PitchDirection    -- ^target goal
                    -> Vector3           -- ^point where to dribble
                    -> Vector3           -- ^nearest opponent
                    -> Float             -- ^dribble value
dribbleValueOnPoint pit dir pt ol = let dt = distanceToThreat (vectorLength (diffVector3 pt ol))
                                        sv = spotValue pit dir pt
                                    in trace ("dribbleValueOnPoint " ++ show (1 - dt) ++ " " ++ show sv) ((1 - dt) * sv)
--                                in (1 - dt) * sv

dribbleValueFromCircle :: PlayerInfo -- ^player
                       -> Vector3    -- ^vector to nearest opponent
                       -> Pitch
                       -> Float      -- ^radius (how far to dribble)
                       -> Float      -- ^angle (where to dribble)
                       -> Float      -- ^value of dribbling to the point
dribbleValueFromCircle p ol pit r a = let pl = location $ plentity p
                                          pt = radiusFromVectorXZ pl r a
                                      in distanceToThreat (vectorLength (diffVector3 pt ol)) * (spotValue pit (direction p) pt)

threatValueFromCircle :: PlayerInfo -> PlayerInfo -> Float -> Float -> Float
threatValueFromCircle p o r a = let pl = location (plentity p)
                                    ol = location (plentity o)
                                in distanceToThreat (vectorLength (diffVector3 (radiusFromVectorXZ pl r a) ol))
