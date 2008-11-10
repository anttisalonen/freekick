module Freekick.Libsoccer.MatchInfo
where

import Data.Maybe
import qualified Data.Map

import Libaddutil.Primitives

import Freekick.Libsoccer.Club
import Freekick.Libsoccer.Player
import Freekick.Libsoccer.Pitch
import Freekick.Libsoccer.Formation
import Freekick.Libsoccer.Lineup
import Freekick.Libsoccer.Stadium
import Freekick.Libsoccer.World

data MatchInfo =
    MatchInfo { homecl        :: Club
              , awaycl        :: Club
              , homepl        :: [Player]
              , awaypl        :: [Player]
              , hformation    :: Formation
              , aformation    :: Formation
              , hlineup       :: Lineup
              , alineup       :: Lineup
              , pitchinfo     :: Pitch
              , starttime     :: DateTime
              }
    deriving (Show, Read, Eq)

getFirstClub :: MatchInfo -> Club
getFirstClub m = homecl m

getAllPlayerIDs :: MatchInfo -> [Integer]
getAllPlayerIDs m = hplids ++ aplids
    where hplids = map getID (homepl m)
          aplids = map getID (awaypl m)

newMatchInfo :: PlayerDB -> Club -> Club -> Stadium -> DateTime -> MatchInfo
newMatchInfo pdb c1 c2 s dt = MatchInfo c1 c2 pl1 pl2 formation442 formation442 (fromJust $ lineup442fromList pl1) (fromJust $ lineup442fromList pl2) pc dt
    where p1  = filterDBByKeys pdb (getPlayerIDs c1)
          p2  = filterDBByKeys pdb (getPlayerIDs c2)
          pc  = testPitch       -- TODO: get the real pitch
          pl1 = (Data.Map.elems p1)
          pl2 = (Data.Map.elems p2)
