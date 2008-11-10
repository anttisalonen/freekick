module Support
where

import Data.Maybe
import Data.List

import Libaddutil.ListUtils
import Libaddutil.Entity
import Libaddutil.Vector
import Libaddutil.Primitives

import Freekick.Libsoccer.Lineup
import Freekick.Libsoccer.Formation
import Freekick.Libsoccer.Player

import Freekick.Libmatch.PlayerInfo
import Freekick.Libmatch.MatchStatus
import Freekick.Libmatch.SoccerPhysics

import Helpers
import Parameters

spotRunDistanceValue :: PlayerInfo -> Vector3 -> Float
spotRunDistanceValue p v = let l = location (plentity p)
                           in max 0 ((maxRunDistance - vectorLength (diffVector3 v l)) / maxRunDistance)

spotInFormationArea :: PlayerInfo -> Lineup -> Formation -> Vector3 -> Bool
spotInFormationArea p l f v = if isNothing ar then False else pointInArea2D (fromJust ar) v
    where ar = getPlayerArea (staticplayer p) l f

-- | Returns a value between 1 (near) and 0 (away) indicating if the player
-- should run to the ball or not.
runToBall :: PlayerInfo -> [PlayerInfo] -> Ball -> Lineup -> Formation -> Float
runToBall p ts b l f = let bl = (location (ballentity b))
                       in if spotInFormationArea p l f bl then 1 else if isNearestToBall p ts b then 1 else 0

-- | Returns a value between 1 (near) and 0 (away) indicating if the player
-- should run to the ball or not depending on distance.
nearToBall :: PlayerInfo -> [PlayerInfo] -> Ball -> Float
nearToBall p ts b = if isNearestToBall p ts b then 1 else spotRunDistanceValue p (location (ballentity b))

isNearestToBall :: PlayerInfo -> [PlayerInfo] -> Ball -> Bool
isNearestToBall p ts b = let tes = map plentity ts
                         in null $ filter (< vectorLength (vectorFromTo (plentity p) (ballentity b)))
                                (map (vectorLength . (vectorFromTo (ballentity b))) tes)

nearestToBall :: MatchStatus -> PlayerInfo
nearestToBall m = nearestPlayerToBall (allPlayingPlayers m) (ball m)

nearestPlayerToBall :: [PlayerInfo] -> Ball -> PlayerInfo
nearestPlayerToBall p b = fst $ mapPreserveMin (\x -> vectorLength (diffVector3 (location $ plentity x) (location $ ballentity b))) p

-- | interposeDistance is the distance in range [0,1] between the player
-- holding the ball (0) and the other one (1).
interposeDistance :: Float
interposeDistance = 0.2

-- TODO: the second player should not simply be the second closest one.
interpose :: PlayerInfo -> [PlayerInfo] -> [PlayerInfo] -> Ball -> Float -> (Vector3, Float)
interpose _ _  _  _ 0.0  = ((0,0,0), 0)
interpose p ts os b maxv = (seekVelocity (plentity p) point maxv, nearToBall p ts b)
    where point = addVector3 ap (scaleVector3 (diffVector3 ap bp) interposeDistance)
          ap    = (scaleVector3 (velocity ae) t) `addVector3` (location ae)
          bp    = (scaleVector3 (velocity be) t) `addVector3` (location be)
          t     = vectorLength (diffVector3 (location (plentity p)) mp) / maxv
          mp    = scaleVector3 (location ae `addVector3` location be) 0.5
          ae    = plentity apl
          be    = plentity bpl
          apl   = nearestPlayerToBall os b
          bpl   = nearestPlayerToBall (filter (\x -> idnum (staticplayer apl) /= idnum (staticplayer x)) os) b
