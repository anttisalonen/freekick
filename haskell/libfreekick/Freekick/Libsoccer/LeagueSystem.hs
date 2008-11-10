module Freekick.Libsoccer.LeagueSystem
where

import Freekick.Libsoccer.Stage
import Libaddutil.Primitives

data LeagueSystem =
     LeagueSystem   {   name           :: String,
                        id             :: Integer,
                        levels         :: [Level],
                        schedule       :: Schedule
                    }
     deriving (Show, Eq)

type Level  = [Branch]
type Branch = (Stage, Integer)       -- league, region

nullLeagueSystem :: LeagueSystem
nullLeagueSystem = LeagueSystem "" 0 [] nullSchedule
