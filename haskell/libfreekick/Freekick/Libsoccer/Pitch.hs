module Freekick.Libsoccer.Pitch
where

import System.IO
import Libaddutil.Primitives
import Libaddutil.Vector

data Pitch = 
    Pitch { pitchname     :: String,
            id            :: Integer,
            friction      :: Float,
            size          :: Area,         -- ^ play area (borders in the image, in pixels)
            area          :: Vector3,      -- ^ image+pitch area (in meters)
            image         :: FilePath
          }
    deriving (Show, Eq, Read)

newPitch :: String -> Pitch
newPitch n = (Pitch n 0 1.0 ((0.0, 0.0, 0.0),(10.0, 0.0, 0.0)) (70.0, 0.0, 100.0) "")

setFriction :: Pitch -> Float -> Pitch
setFriction p f =
    p{friction = f}

getFriction :: Pitch -> Float
getFriction p = friction p

testPitch :: Pitch
testPitch = newPitch "name"

substitutesBench :: Pitch 
                 -> Bool     -- ^true for home club
                 -> Area
substitutesBench p h = if h then ((-51,0,pz/2+10),(-50, 0, pz/2 + 10)) else ((-51,0,pz/2-10),(-50, 0, pz/2 - 10))
    where (_,_,pz) = area p
