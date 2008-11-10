module Actions
where

import qualified Data.Map
import Debug.Trace
import Data.List
import Data.Maybe

import Libaddutil.Entity
import Libaddutil.Vector
import Libaddutil.Primitives

import Freekick.Libsoccer.Pitch
import Freekick.Libsoccer.Lineup
import Freekick.Libsoccer.Formation
import Freekick.Libsoccer.Player

import Freekick.Libmatch.PlayerInfo
import Freekick.Libmatch.MatchStatus
import Freekick.Libmatch.Control
import Freekick.Libmatch.Rules
import Freekick.Libmatch.SoccerPhysics

import Common
import Dribble
import Helpers
import Parameters
import Pass
import Support


withBallAction :: PlayerInfo    -- ^Player self
               -> [PlayerInfo]  -- ^Team mates
               -> [PlayerInfo]  -- ^Opponents
               -> Pitch
               -> Ball
               -> BallPlay
               -> String
withBallAction p ts os pit b bp = case position (staticplayer p) of
                                    Goalkeeper -> if vectorLength ((velocity . ballentity) b) > 1.0 
                                                    then actionToString p (HoldBallAIAction (getGoal pit (other (direction p))))
                                                    else actionToString p (snd (Data.Map.findMax amap))
                                    _          -> trace ("withBallAction " ++ show values) actionToString p (snd (Data.Map.findMax amap))
--                                    _          -> actionToString p (snd (Data.Map.findMax amap))
    where ploc         = (location . plentity) p
          passcheck    = bestPass p ts os pit
          scorecheck   = spotValue pit (direction p) ploc * passPathSafety (map (location . plentity) os) ploc (opposingGoal p pit)
          dribblecheck = case bp of
                           BallIn _ -> bestDribbleValue p os pit
                           _        -> ((0,0,0), -1)
          values       = [fst (snd passcheck), scoreCoefficient * scorecheck, dribbleCoefficient * (snd dribblecheck)]
          actions      = [PassAIAction (fst passcheck) (snd (snd passcheck)) b, ScoreAIAction pit b, DribbleAIAction (fst dribblecheck) b]
          amap         = Data.Map.fromList (zip values actions)

withoutBallAction :: PlayerInfo -> MatchStatus -> String
withoutBallAction p m = case position (staticplayer p) of
                          Goalkeeper -> if isNothing ballgoal 
                                          then actionToString p (snd (Data.Map.findMax amap)) 
                                          else actionToString p (GotoPointAIAction (fst (fromJust ballgoal)))
                          _          -> actionToString p (snd (Data.Map.findMax amap))
    where ts         = getPlayingTeammatesFromMatchStatus m p
          os         = getPlayingOpposingTeam m p
          f          = playerFormation m p
          l          = playerLineup m p
          b          = ball m
          pit        = pitch m
          fetchcheck = runToBall p ts b l f
          formcheck  = if not (isNothing ar) && inArea (fromJust ar) ((location . plentity) p) then 0 else 0.2
          staycheck  = 0.1
          intercheck = if ncl == direction p || position (staticplayer p) == Goalkeeper 
                         then ((location $ ballentity b), -1) 
                         else interpose p ts os b maxPlayerVelocity
          ncl        = direction npl
          npl        = nearestToBall m
          ballgoal   = ballMovingToGoal pit b (other (direction p))
          ar         = getPlayerArea (staticplayer p) l f
          values     = [fetchcheck, (snd intercheck), formcheck, staycheck]
          actions    = [FetchAIAction (ballentity b), GotoPointAIAction (fst intercheck), GotoFormationAIAction pit f l, StayAIAction]
          amap       = Data.Map.fromList (zip values actions)

actionToString :: PlayerInfo -> AIAction -> String
actionToString p a = case a of
                       PassAIAction _ v b            -> dribbleBallAction p v b 5     -- TODO: make note of the target player + variable angle
                       ScoreAIAction pit b           -> scoreBallAction p pit b
                       DribbleAIAction t b           -> dribbleBallAction p t b 0
                       FetchAIAction e               -> runToEntityAction p e
                       GotoFormationAIAction pit f l -> runToFormationAction p pit f l
                       StayAIAction                  -> standAction p
                       GotoPointAIAction t           -> runToPointAction p t
                       HoldBallAIAction t            -> holdBallToPointAction p t
                                                           -- TODO: add future actions here

dribbleBallAction :: PlayerInfo -- ^Player self
                  -> Vector3    -- ^target
                  -> Ball       -- ^ball
                  -> Float      -- ^kick angle in degrees
                  -> String
dribbleBallAction p t b a = kickToEvent (x,y,z) plid
    where (x,y,z)     = ballVelocityToTarget (ballentity b) t a
          plid        = idnum (staticplayer p)

passBallAction :: PlayerInfo    -- ^Player self
               -> PlayerInfo    -- ^Target team mate
               -> Ball
               -> String
passBallAction p t b = dribbleBallAction p (location (plentity t)) b 5 -- TODO: variable angle?

standAction :: PlayerInfo -> String
standAction p = runToEvent (scaleVector3 (velocity (plentity p)) (-1)) (idnum (staticplayer p))

scoreBallAction :: PlayerInfo -> Pitch -> Ball -> String
scoreBallAction p pit b = trace ("scoreBallAction " ++ show (x,y,z)) (kickToEvent (x, y, z) plid)
    where ta         = 25       -- TODO: variable angle
          tv         = opposingGoal p pit
          (x,y,z)    = ballVelocityToTarget (ballentity b) tv ta
          plid       = idnum (staticplayer p)

runToEntityAction :: PlayerInfo -> Entity -> String
runToEntityAction p e = runToEvent (x, y, z) plid
    where y        = 0
          (x,_, z) = velocityPursuitEntity (plentity p) e maxPlayerVelocity
          plid     = idnum (staticplayer p)

runToPointAction :: PlayerInfo -> Vector3 -> String
runToPointAction p (x,_,z) = runToEvent (x,0,z) plid
    where plid = idnum (staticplayer p)

holdBallToPointAction :: PlayerInfo -> Vector3 -> String
holdBallToPointAction p (x,_,z) = holdBallToEvent (x,0,z) plid
    where plid = idnum (staticplayer p)

runToFormationAction :: PlayerInfo -> Pitch -> Formation -> Lineup -> String
runToFormationAction p pit f l = runToEvent (x,y,z) plid
    where y        = 0
--          ar       = getPlayerArea (staticplayer p) l f
          (x,_, z) = playerVelocityToArea (formationSpot p pit f l) 0.5 (plentity p) -- TODO: make energy to use variable
          plid     = idnum (staticplayer p)

-- | If the distance between player's kick off formation spot and the kickoff
-- spot is less than this, player should go do the kickoff himself.
formationKickoffDistance :: Float
formationKickoffDistance = 15.0

-- | Distance [0,1] between kickoff spot and formation spot before giving kickoff.
kickoffSpotDistance :: Float
kickoffSpotDistance = 0.2

runToKickoffFormationAction :: PlayerInfo 
                            -> Pitch 
                            -> Formation 
                            -> Lineup 
                            -> PitchDirection           -- ^ who gets to do the kickoff
                            -> String
runToKickoffFormationAction p pit f l d = runToEvent (x, y, z) plid
    where y        = 0
          fs       = kickoffFormationSpot p pit f l
          nfs      = addVector3 fs (scaleVector3 (normalizeVector (diffVector3 fs (getCentreSpot pit))) nearCentreSpotRadius)
          kicksoff = direction p == d && vectorLength (diffVector3 fs (getCentreSpot pit)) < formationKickoffDistance
          kovec    = diffVector3 (moveTo kickoffSpotDistance (getCentreSpot pit) fs) ((location . plentity) p)
          tv       = if (direction p == d) then fs else if vectorNearCentreSpot pit fs then nfs else fs
          (x,_, z) = if kicksoff then kovec else playerVelocityToArea tv 0.5 (plentity p) -- TODO: make energy to use variable
          plid     = idnum (staticplayer p)

throwinSupportAction :: MatchStatus -> PlayerInfo -> PitchDirection -> String
throwinSupportAction m p d = standAction p    -- TODO

