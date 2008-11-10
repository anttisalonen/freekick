module Libaddutil.Physics
where

import Libaddutil.Vector
import Libaddutil.Entity
import Libaddutil.Misc

import Data.List

applyFriction :: Float -> Float -> Float -> Entity -> Entity
applyFriction friction viscosity interval e = e{velocity = newvel}
    where oldvel = velocity e
          newvel = if onground
                     then addVector3 oldvel (scaleVector3 (velocity e)                                (-friction * interval))
                     else addVector3 oldvel (scaleVector3 (entityViscosity (-viscosity) (velocity e)) interval)
          onground = y < 0.1
          (_,y,_)  = location e

applyGravity :: Float -> Entity -> Entity
applyGravity g e = e{acceleration=newacc}
    where newacc  = if y > 0 then addVector3 (acceleration e) (0,g,0) else acceleration e
          (_,y,_) = location e

-- | Calculates drag force with viscosity coefficient and velocity.
entityViscosity :: Float -> Vector3 -> Vector3
entityViscosity viscosity vel = scaleVector3 vel viscosity

-- |'restrictEntity' makes things bounce from the ground. Float is the coefficient
-- of restitution (1 for elastic, 0 for inelastic collision).
restrictEntity :: Float -> Entity -> Entity
restrictEntity cor e = e{location=newloc, velocity=newvel,acceleration=newacc}
    where newloc     = if bounced then (x,0,z) else location e
          newvel     = if bounced then if not veryslow then (vx, -vy*cor,vz) else (vx,0,vz) else velocity e
          newacc     = if bounced then if not veryslow then (ax, -ay*cor,az) else (ax,0,az) else acceleration e
          (x,y,z)    = location e
          (vx,vy,vz) = velocity e
          (ax,ay,az) = acceleration e
          bounced    = y < 0.0
          veryslow   = abs vy < 0.1

-- | Updates an entity based on physics.
updateEntityWithPhysics :: Float            -- ^time interval (delta)
                        -> Float            -- ^gravity
                        -> Float            -- ^friction
                        -> Float            -- ^air viscosity
                        -> Float            -- ^coefficient of restitution
                        -> Float            -- ^maximum velocity (0 -> unlimited)
                        -> Entity           -- ^entity itself
                        -> Entity
updateEntityWithPhysics i g f v r m e = restrictEntity r $ applyFriction f v i $ applyGravity g $ updateEntityMax m i e

locationInOneSecond :: Entity -> Vector3
locationInOneSecond e = location (updateEntity 1 e)

locationInTime :: Float -> Entity -> Vector3
locationInTime t e = location (updateEntity t e)

-- | Without air resistance.
landingSpot :: Float -> Entity -> Vector3
landingSpot g e = let ( _,ly,_)  = location e
                      ( _,vy,_)  = velocity e
                      lt         = quadr (g/2) vy ly
                      landtime   = if not (null lt) then maximum lt else 0.0
                  in if ly < 0 
                       then (location e)
                       else if landtime <= 0
                              then (location e)
                              else flyingPath g landtime e

flyingPath :: Float   -- ^Gravity
           -> Float   -- ^Time delta in seconds from current time
           -> Entity  -- ^Flying entity
           -> Vector3 -- ^Location of the entity at the given time
flyingPath g t e = let (vx,vy,vz) = velocity e
                   in addVector3 (location e) (vx * t, vy * t + (g/2)*t**2, vz * t)
