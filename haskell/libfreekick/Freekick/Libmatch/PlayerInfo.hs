module Freekick.Libmatch.PlayerInfo
where

import Freekick.Libmatch.Rules

import Freekick.Libsoccer.Player
import Freekick.Libsoccer.Lineup
import Freekick.Libsoccer.Pitch

import Libaddutil.Entity

import qualified Data.Map

type Players = Data.Map.Map Integer PlayerInfo

data PlayerInfo = PlayerInfo { staticplayer :: Player
                             , plentity     :: Entity
                             , playerstatus :: PlayerStatus
                             , direction    :: PitchDirection
                             , ishomeclub   :: Bool
                             }
    deriving (Eq, Show, Read)

instance Entital PlayerInfo where
    getEntity = plentity

data PlayerStatus = Playing
                  | Injured
                  | Substitute
                  | Observer
    deriving (Eq, Show, Read)

minPlayerAcceleration :: Float   -- ^ [m/s^2]
minPlayerAcceleration = 0.0

maxPlayerAcceleration :: Float   -- ^ [m/s^2]
maxPlayerAcceleration = 150.0

minPlayerVelocity :: Float       -- ^ [m/s]
minPlayerVelocity = 0.0

maxPlayerVelocity :: Float       -- ^ [m/s]
maxPlayerVelocity = 8.0

maxKickDistance :: Float         -- ^ [m]
maxKickDistance = 1.5

playerWidth :: Float             -- ^ [m]
playerWidth = 1.0

playerDepth :: Float             -- ^ [m]
playerDepth = 0.5

createPlayerInfo :: PitchDirection -> Lineup -> Player -> PlayerInfo
createPlayerInfo dir l p = PlayerInfo p (Entity (fromInteger lx, 0, fromInteger lz) (0, 0, 0) (0, 0, 0)) ps dir hm
    where lx = (idnum p) `mod` 52
          lz = (idnum p) `mod` 47
          ps = if playerIsAssigned l p then Playing else if playerIsSubstitute l p then Substitute else Observer
          hm = dir == Up

getTeammatesFromList :: [PlayerInfo] -> PlayerInfo -> [PlayerInfo]
getTeammatesFromList pls p = filter (\x -> ishomeclub x == ishomeclub p && idnum (staticplayer x) /= idnum (staticplayer p)) pls

onOwnSide :: PlayerInfo -> Pitch -> Bool
onOwnSide p pit = let d       = direction p
                      (_,_,z) = location $ plentity p
                  in if d == Up then z < getMidfieldLine pit else z > getMidfieldLine pit

getPlayerPositionInt :: PlayerInfo -> Int
getPlayerPositionInt p = positionToInt (position (staticplayer p))
