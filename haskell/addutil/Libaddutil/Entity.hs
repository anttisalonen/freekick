module Libaddutil.Entity
where

import Libaddutil.Vector

class Eq a => Entital a where
    getEntity :: a -> Entity

data Entity = Entity { location     :: Vector3
                     , velocity     :: Vector3
                     , acceleration :: Vector3
                     }
    deriving (Eq, Show, Read)

setEntityLocation :: Entity -> Vector3 -> Entity
setEntityLocation e l = e{location = l}

nullEntity :: Entity
nullEntity = Entity nullVector3 nullVector3 nullVector3

stopEntity :: Entity -> Entity
stopEntity e = e{velocity=(0,0,0),acceleration=(0,0,0)}

updateEntity :: Float -> Entity -> Entity
updateEntity i e = e{location=nl,velocity=nv}
    where nl = (location e) `addVector3` ((velocity e) `scaleVector3` i)
          nv = (velocity e) `addVector3` ((acceleration e) `scaleVector3` i)

updateEntityMax :: Float -> Float -> Entity -> Entity
updateEntityMax mx i e | mx <= 0   = updateEntity i e
                       | otherwise = updateEntity i ne
    where ne = e{velocity = clampVector 0 mx (velocity e)}

setEntityAcceleration :: Entity -> Vector3 -> Entity
setEntityAcceleration e a = e{acceleration = a}

setEntityVelocity :: Entity -> Vector3 -> Entity
setEntityVelocity e v = e{velocity = v}

vectorFromTo :: Entity -> Entity -> Vector3
vectorFromTo e1 e2 = (location e2) `diffVector3` (location e1)

