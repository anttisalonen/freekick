module Freekick.Libmatch.SoccerPhysics
where

import Data.Maybe

import Libaddutil.Vector
import Libaddutil.Entity
import Libaddutil.Collision
import Libaddutil.Person
import Libaddutil.Physics
import Libaddutil.Primitives
import Libaddutil.Misc

import Freekick.Libsoccer.Player
import Freekick.Libsoccer.Pitch
import Freekick.Libsoccer.Common

import Freekick.Libmatch.Rules
import Freekick.Libmatch.PlayerInfo
import Freekick.Libmatch.MatchStatus

gravity :: Float
gravity = (-9.8)              -- ^[m/s^2]

ballFriction :: Float
ballFriction = 0.8

playerFriction :: Float
playerFriction = 1.5

ballAirViscosity :: Float
ballAirViscosity = 0.7

playerAirViscosity :: Float
playerAirViscosity = 2.0

playerCoR :: Float            -- coefficient of restitution
playerCoR = 0.8

ballCoR :: Float
ballCoR = 0.2

okVelocityRange :: Float
okVelocityRange = 1.0

okLocationRange :: Float
okLocationRange = 0.5

okAreaRange :: Float
okAreaRange = 6.0

playerAccelerationMultiplier :: Float
playerAccelerationMultiplier = 5.0

maxBallKickVelocity :: Float
maxBallKickVelocity = 25          -- ^ [m/s^2]

playerCollisionBoundaryMove :: Float
playerCollisionBoundaryMove = 0.15

ballKickCoefficient :: Float
ballKickCoefficient = 2.0

setPlayerLocation :: Vector3 -> PlayerInfo -> PlayerInfo
setPlayerLocation x p = p{plentity = setEntityLocation (plentity p) x}

setPlayerVelocity :: Vector3 -> PlayerInfo -> PlayerInfo
setPlayerVelocity v p = p{plentity = setEntityVelocity (plentity p) ma}
    where ma = clampVector 0 maxPlayerVelocity v

setPlayerAcceleration :: Vector3 -> PlayerInfo -> PlayerInfo
setPlayerAcceleration a p = p{plentity = setEntityAcceleration (plentity p) ma}
    where ma = clampVector minPlayerAcceleration maxPlayerAcceleration a

setBallLocation :: Vector3 -> Ball -> Ball
setBallLocation p b = b{ballentity = stopEntity (setEntityLocation (ballentity b) p)}

setBallVelocity :: Vector3 -> Ball -> Ball
setBallVelocity v b = b{ballentity = setEntityVelocity (ballentity b) va}
    where va = clampVector 0 maxBallKickVelocity v

setBallAcceleration :: Vector3 -> Ball -> Ball
setBallAcceleration a b = b{ballentity = setEntityAcceleration (ballentity b) a}

-- | Returns the velocity vector the player should have in order to move to
-- the target location with given energy, i.e. 0=slow walk, 1=hurry.
-- The result is a simple vector to the target without pursuit.
playerVelocityToTarget :: Vector3  -- ^ target location
                       -> Float    -- ^ energy level
                       -> Entity   -- ^ entity to move
                       -> Vector3  -- ^ should-be velocity
playerVelocityToTarget newl v e = playerVelocityToTarget' newl v e okLocationRange

playerVelocityToTarget' :: Vector3 -> Float -> Entity -> Float -> Vector3
playerVelocityToTarget' newl v e r = if abs (vectorLength ldif) < r
                                       then (0,0,0)
                                       else scaleVector3 ldif (v * maxPlayerVelocity)
    where oldl = location e
          ldif = diffVector3 newl oldl

seekVelocity :: Entity -> Vector3 -> Float -> Vector3
seekVelocity s t maxv = dv `diffVector3` (velocity s)
    where dv = normalizeVector (t `diffVector3` (location s)) `scaleVector3` maxv

-- | The higher, the longer lookahead time when fetching the ball
lookaheadTimeCoefficient :: Float
lookaheadTimeCoefficient = 0.2

velocityPursuitEntity :: Entity -> Entity -> Float -> Vector3
velocityPursuitEntity s t maxv = seekVelocity s ((location t) `addVector3` ((velocity t) `scaleVector3` lt)) maxv
    where lt = lookaheadTimeCoefficient * (vectorLength (diffVector3 (location t) (location s)) / (maxv + vectorLength (velocity t)))

-- | Returns the velocity vector the player should have in order to move to
-- the vicinity of target with given energy, i.e. 0=slow walk, 1=hurry.
playerVelocityToArea :: Vector3 -> Float -> Entity -> Vector3
playerVelocityToArea newl v e = playerVelocityToTarget' newl v e okAreaRange

acceleratePlayerToVelocity :: Vector3 -> PlayerInfo -> PlayerInfo
acceleratePlayerToVelocity newv p = if abs (vectorLength vdif) < okVelocityRange
                                      then p
                                      else setPlayerAcceleration newa p
    where vdif = diffVector3 newv oldv
          oldv = velocity $ plentity p
          newa = scaleVector3 vdif playerAccelerationMultiplier

-- | Gives the velocity of the E needed to land E to the landing point.
-- TODO: doesn't take air resistance into account.
ballVelocityToTarget :: Entity       -- ^ entity to kick
                     -> Vector3      -- ^ target landing point (y discarded).
                     -> Float        -- ^ the angle of kick in degrees, >= 0.
                     -> Vector3      -- ^ needed force
ballVelocityToTarget e (tx,_,tz) ta = let (blx,_  ,blz) = location e
                                          ldif          = (tx - blx, 0, tz - blz)
                                          ldiflen       = vectorLength ldif
                                          angle         = if ta < 0 then 0 else ta * pi / 180
                                          xzvel         = if ta > 0 && ta < 90
                                                            then ballKickCoefficient * sqrt ((ldiflen * (-gravity)) / (2 * tan angle))
                                                            else ballKickCoefficient * ldiflen           -- TODO: improve?
                                          xzangle       = atan2 (tz - blz) (tx - blx)
                                          yvel          = xzvel * tan angle
                                          xvel          = xzvel * cos xzangle
                                          zvel          = xzvel * sin xzangle
                                      in if ldiflen == 0 then (0,0,0) else (xvel,yvel,zvel)

stopBall :: Ball -> Ball
stopBall b = b{ballentity = stopEntity (ballentity b)}

updateBall :: Float -> Ball -> Ball
updateBall i b = b{ballentity=updateEntityWithPhysics i gravity ballFriction ballAirViscosity ballCoR 0 (ballentity b)}

updatePlayer :: Float -> PlayerInfo -> PlayerInfo
updatePlayer i p = p{plentity=updateEntityWithPhysics i gravity playerFriction playerAirViscosity playerCoR maxPlayerVelocity (plentity p)}

updatePlayerEntity :: Float -> Entity -> Entity
updatePlayerEntity = updateEntityMax maxPlayerVelocity

updateBallEntity :: Float -> Entity -> Entity
updateBallEntity = updateEntity

ballBoundingBox :: Ball -> Area
ballBoundingBox b = let (mx,my,mz) = location $ ballentity b
                        tl         = (mx - ballRadius / 2, my, mz - ballRadius / 2)
                        br         = (mx + ballRadius / 2, my + ballRadius, mz + ballRadius / 2)
                    in (tl, br)

playerBoundingBox :: PlayerInfo -> Area
playerBoundingBox p = let (mx,my,mz) = location $ plentity p
                          heig = height $ personal $ staticplayer p
                          tl = (mx - playerWidth / 2, my                      , mz - playerWidth / 2)
                          br = (mx + playerWidth / 2, my + (toEnum heig / 100), mz + playerWidth / 2)
                      in (tl, br)

playerIntersect :: PlayerInfo -> PlayerInfo -> Maybe PlayerInfo
playerIntersect p1 p2 = if checkPlayerIntersect p1 p2 then Just p2 else Nothing

checkPlayerIntersect :: PlayerInfo -> PlayerInfo -> Bool
checkPlayerIntersect p1 p2 = boxIntersect (playerBoundingBox p1) (playerBoundingBox p2)

playerCollideWith :: PlayerInfo -> PlayerInfo -> PlayerInfo
playerCollideWith p1 p2 = if not (checkPlayerIntersect p1 p2) then p1 else 
                              p1{plentity=makeCollision (plentity p1) (plentity p2) playerCoR c1 c2 playerCollisionBoundaryMove}
    where c1  = getCentreOfMass p1
          c2  = getCentreOfMass p2

makeCollision :: Entity    -- ^ Entity that will be moved
              -> Entity    -- ^ Other colliding party
              -> Float     -- ^ coefficient of restitution
              -> Vector3   -- ^ centre of mass 1
              -> Vector3   -- ^ centre of mass 2
              -> Float     -- ^ amount of boundary move
              -> Entity    -- ^ moved & collided entity
makeCollision e1 e2 cor c1 c2 bm = let l1 = location e1
                                       dv = diffVector3 c1 c2
                                       (nlx, _, nlz) = addVector3 l1 (scaleVector3 dv bm)
                                       nv = addVector3 (velocity e1) (scaleVector3 (velocity e2) ((cor - 1) * (-0.5)))
                                   in e1{location = (nlx, getY l1, nlz), velocity = nv, acceleration = (0,0,0)}

getCentreOfMass :: PlayerInfo -> Vector3
getCentreOfMass p = (x,y,z)
    where (x,y1,z) = location $ plentity p
          h        = 0.01 * (toEnum $ height $ personal $ staticplayer p)
          y        = y1 + h/2

getBallCentreOfMass :: Ball -> Vector3
getBallCentreOfMass b = (x,y,z)
    where (x,ny,z) = location $ ballentity b
          y        = ny + ballRadius

playerHandleCollisionWithOthers :: [PlayerInfo] -> PlayerInfo -> PlayerInfo
playerHandleCollisionWithOthers pn p = let rpn = filter (\x -> (idnum . staticplayer) x /= (idnum . staticplayer) p) pn
                                       in foldl playerCollideWith p rpn

checkBallPlayerIntersect :: Ball -> PlayerInfo -> Bool
checkBallPlayerIntersect b p = boxIntersect (ballBoundingBox b) (playerBoundingBox p)

ballCollideWith :: (Ball, PitchDirection) -> PlayerInfo -> (Ball, PitchDirection)
ballCollideWith (b, d) p = if not (checkBallPlayerIntersect b p) then (b,d) else 
                               (b{ballentity=makeCollision (ballentity b) (plentity p) ballCoR cb cp playerCollisionBoundaryMove}, (direction p))
    where cb = getBallCentreOfMass b
          cp = getCentreOfMass p

ballHandleCollisions :: [PlayerInfo] -> (Ball, PitchDirection) -> (Ball, PitchDirection)
ballHandleCollisions p b = foldl ballCollideWith b p

getPlayerMeetingTime :: Float            -- ^ball velocity after pass
                     -> Entity           -- ^(moving) entity to whom to pass
                     -> Maybe Float      -- ^time in s when e meets ball
getPlayerMeetingTime v e = let ts = quadr a b c 
                               (vx, _, vz) = velocity e
                               (px, _, pz) = location e
                               a = vx + vz - v
                               b = 2 * px * vx + 2 * pz * vz
                               c = px**2 + pz**2
                               ts2 = filter (>=0) ts
                               ts3 = minimum ts2
                           in if null ts2 then Nothing else Just ts3

getPassVector :: Float                -- ^ball passing velocity
              -> Entity               -- ^(moving) entity to whom to pass
              -> Maybe Vector3        -- ^vector where the ball meets e
getPassVector b e = let mt = getPlayerMeetingTime b e 
                        t = fromMaybe 0 mt
                        x = px + vx * t
                        z = pz + vz * t
                        (vx, _, vz) = velocity e
                        (px, _, pz) = location e
                    in if isNothing mt then Nothing else Just (x, 0, z)

-- ^Returns time in s when ball passes the goal line if it keeps moving
-- like it does now.
-- TODO: - doesn't take ball height into account.
--       - doesn't take rolling friction or drag into account.
--       - takes balls coming from the other side (outside the pitch)
--         into account.
ballMovingToGoal :: Pitch 
                 -> Ball 
                 -> PitchDirection         -- ^which goal
                 -> Maybe (Vector3, Float) -- ^(place, time in s when ball meets goal)
                                           -- nothing when won't meet
ballMovingToGoal p b d = let l         = segmentSegmentIntersection ballnow ballthen leftpost rightpost
                             ballnow   = location (ballentity b)
                             ballthen  = locationInTime 60 (ballentity b)
                             leftpost  = getGoalPost p d LeftSide
                             rightpost = getGoalPost p d RightSide
                             diffdist  = vectorLength $ (fromJust l) `diffVector3` ballnow
                             t         = diffdist / (vectorLength (velocity (ballentity b)))
                         in if isNothing l then Nothing else Just (fromJust l, t)
