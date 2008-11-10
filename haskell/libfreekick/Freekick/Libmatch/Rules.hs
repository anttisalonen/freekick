module Freekick.Libmatch.Rules
where

import Libaddutil.Entity
import Libaddutil.Vector
import Libaddutil.Primitives

import Freekick.Libsoccer.Pitch
import Freekick.Libsoccer.Common

data StatusType = Pause
                | Time
                | Score
                | Match
     deriving (Show, Eq, Read)

data PitchDirection = Up
                    | Down
     deriving (Show, Eq, Read)

goalHeight :: Float
goalHeight = 2.44

goalWidth :: Float
goalWidth = 7.32

penaltyKickDistance :: Float
penaltyKickDistance = 11.0

inFieldOfPlay :: Entity -> Pitch -> Bool
inFieldOfPlay e p = lineCrossed e p == None

upperPenaltySpot :: Pitch -> Vector3
upperPenaltySpot p = (getMidLine p, penaltyKickDistance, 0)

lowerPenaltySpot :: Pitch -> Vector3
lowerPenaltySpot p = (getMidLine p, getBottomLine p - penaltyKickDistance, 0)

penaltyBoxHeight :: Float
penaltyBoxHeight = 16.5

penaltyBoxWidth :: Float
penaltyBoxWidth = 40.3

inUpperPenaltyBox :: Entity -> Pitch -> Bool
inUpperPenaltyBox e p = inFieldOfPlay e p && inBox (location e) (getMidLine p - (penaltyBoxWidth / 2), 0, 0) (getMidLine p + (penaltyBoxWidth / 2), penaltyBoxHeight, 0)

inLowerPenaltyBox :: Entity -> Pitch -> Bool
inLowerPenaltyBox e p = inFieldOfPlay e p && inBox (location e) (getMidLine p - (penaltyBoxWidth / 2), getBottomLine p - penaltyBoxHeight, 0) (getMidLine p + (penaltyBoxWidth / 2), getBottomLine p, 0)

inPenaltyBox :: Entity -> Pitch -> Bool
inPenaltyBox e p = inUpperPenaltyBox e p || inLowerPenaltyBox e p

inPenaltyBoxDir :: Entity -> PitchDirection -> Pitch -> Bool
inPenaltyBoxDir e d p | d == Up   = inUpperPenaltyBox e p
                      | otherwise = inLowerPenaltyBox e p

goalAreaHeight :: Float
goalAreaHeight = 5.5

goalAreaWidth :: Float
goalAreaWidth = 18.32

centreSpot :: Pitch -> Vector3
centreSpot p = (getMidLine p, 0, getMidfieldLine p)

centreSpotRadius :: Float
centreSpotRadius = 9.15

nearCentreSpotRadius :: Float
nearCentreSpotRadius = centreSpotRadius * 1.4

penaltyCircleRadius :: Float
penaltyCircleRadius = 9.15

inUpperGoalArea :: Entity -> Pitch -> Bool
inUpperGoalArea e p = inFieldOfPlay e p && inBox (location e) (getMidLine p - (goalAreaWidth / 2), 0, 0) (getMidLine p + (goalAreaWidth / 2), goalAreaHeight, 0)

inLowerGoalArea :: Entity -> Pitch -> Bool
inLowerGoalArea e p = inFieldOfPlay e p && inBox (location e) (getMidLine p - (goalAreaWidth / 2), getBottomLine p - goalAreaHeight, 0) (getMidLine p + (goalAreaWidth / 2), getBottomLine p, 0)

withinUpperPenaltyKickCircle :: Entity -> Pitch -> Bool
withinUpperPenaltyKickCircle e p = inCircle (location e) (upperPenaltySpot p) penaltyCircleRadius

withinLowerPenaltyKickCircle :: Entity -> Pitch -> Bool
withinLowerPenaltyKickCircle e p = inCircle (location e) (lowerPenaltySpot p) penaltyCircleRadius

withinPenaltyKickCircle :: Entity -> Pitch -> Bool
withinPenaltyKickCircle e p = withinUpperPenaltyKickCircle e p || withinLowerPenaltyKickCircle e p

nearCentreSpot :: Pitch -> Entity -> Bool
nearCentreSpot p e = vectorNearCentreSpot p (location e)

vectorNearCentreSpot :: Pitch -> Vector3 -> Bool
vectorNearCentreSpot p v = inCircle v (centreSpot p) nearCentreSpotRadius

inCentreSpot :: Pitch -> Entity -> Bool
inCentreSpot p e = vectorInCentreSpot p (location e)

vectorInCentreSpot :: Pitch -> Vector3 -> Bool
vectorInCentreSpot p v = inCircle v (centreSpot p) centreSpotRadius

onPitch :: Pitch -> Entity -> Bool
onPitch p e = inBox (0,0,0) (area p) (location e)

inUpperGoal :: Entity -> Pitch -> Bool
inUpperGoal e p = lineCrossed e p == Goal Up

inLowerGoal :: Entity -> Pitch -> Bool
inLowerGoal e p = lineCrossed e p == Goal Down

inGoal :: Entity -> Pitch -> Bool
inGoal e p = inUpperGoal e p || inLowerGoal e p

getMidfieldLine :: Pitch -> Float
getMidfieldLine p = getZ (area p) / 2

getMidLine :: Pitch -> Float
getMidLine p = getX (area p) / 2

getCentreSpot :: Pitch -> Vector3
getCentreSpot p = (getMidLine p, 0, getMidfieldLine p)

getRightLine :: Pitch -> Float
getRightLine p = getX (area p)

getBottomLine :: Pitch -> Float
getBottomLine p = getZ (area p)

getSide :: PitchSide -> Pitch -> Float
getSide s p = if s == LeftSide then 0 else getRightLine p

getCorner :: Pitch -> PitchDirection -> PitchSide -> Vector3
getCorner p d s = (x,0,z)
    where x = if s == LeftSide then 0 else getRightLine p
          z = if d == Up       then 0 else getBottomLine p

getGoalkickPoint :: Pitch -> PitchDirection -> PitchSide -> Vector3
getGoalkickPoint p d s = (x,0,z)
    where x = if s == LeftSide then (getMidLine p) - goalAreaWidth / 2 else (getMidLine p) + goalAreaWidth / 2
          z = if d == Down     then goalAreaHeight else (getBottomLine p) - goalAreaHeight

getGoal :: Pitch -> PitchDirection -> Vector3
getGoal p d = (getMidLine p,0,z)
    where z = if d == Up then 0 else getBottomLine p

getGoalPost :: Pitch -> PitchDirection -> PitchSide -> Vector3
getGoalPost p d s = (x,0,z)
    where x = if s == LeftSide then (getMidLine p) - goalWidth / 2 else (getMidLine p) + goalWidth / 2
          z = if d == Down     then 0                              else getBottomLine p

readyForKickoff :: [Vector3] -> [Vector3] -> Pitch -> Bool
readyForKickoff us ds p = length us == 11 && length ds == 11 && and (map (onOwnSideV Up p) us) && and (map (onOwnSideV Down p) ds)

onOwnSideV :: PitchDirection -> Pitch -> Vector3 -> Bool
onOwnSideV d p (_,_,z) | d == Up   = z <= getMidfieldLine p
                       | otherwise = z >= getMidfieldLine p

data LineCrossing = None
                  | Side PitchSide
                  | Goal PitchDirection
                  | End PitchDirection PitchSide
     deriving (Show, Eq, Read)

lineCrossed :: Entity -> Pitch -> LineCrossing
lineCrossed e p = if inl && inr && inu && ind
                      then None
                      else if not inu && right_from_post && left_from_post
                               then Goal Up
                               else if not ind && right_from_post && left_from_post
                                        then Goal Down
                                        else if inl && inr && not inu && right_from_post && not left_from_post
                                                 then End Up RightSide
                                                 else if inl && inr && not inu && left_from_post && not right_from_post
                                                          then End Up LeftSide
                                                          else if not ind && right_from_post && not left_from_post
                                                                   then End Down RightSide
                                                                   else if not ind && left_from_post && not right_from_post
                                                                            then End Down LeftSide
                                                                            else if not inl
                                                                                     then Side LeftSide
                                                                                     else Side RightSide
    where inl = x >= 0
          inr = x <= px
          inu = z >= 0
          ind = z <= pz
          right_from_post = x > (px / 2) - (goalWidth / 2)
          left_from_post  = x < (px / 2) + (goalWidth / 2)
          (x,  _,  z)  = location e
          (px, _,  pz) = area p

withinInnerPitch :: Entity -> Pitch -> Bool
withinInnerPitch e p = x < getRightLine p && x > 0 && y < getBottomLine p && y > 0
    where (x,_,y) = location e

-- |BallPlay can hold the information on the current status of the play. The
-- 'Bool' after 'BallOut' is set to True if the play is blocked, i.e. not
-- given free by the referee (e.g. free kick with opposing players too near
-- the ball.) 'PitchDirection' is the club in control while 'Vector3'
-- refers to the point where to resume play.
data BallPlay = BallIn PitchDirection
              | BallOut BallOutType PitchDirection Vector3 Bool
    deriving (Show, Eq, Read)

data BallOutType = PreKickoff
                 | Kickoff
                 | Throwin
                 | Goalkick
                 | Cornerkick
                 | IndirectFreekick
                 | DirectFreekick
                 | Penaltykick
                 | DroppedBall
                 | HalfFullTime
    deriving (Show, Eq, Read)

other :: PitchDirection -> PitchDirection
other Up = Down
other Down = Up

numPlayers :: Int
numPlayers = 22

changeBallOwner :: PitchDirection -> BallPlay -> BallPlay
changeBallOwner pd b = case b of 
                         BallIn _        -> BallIn pd
                         BallOut a _ v t -> BallOut a pd v t

ballInPlay :: BallPlay -> Bool
ballInPlay b = case b of
                 BallIn _ -> True
                 _        -> False
