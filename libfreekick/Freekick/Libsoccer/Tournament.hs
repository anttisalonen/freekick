module Freekick.Libsoccer.Tournament
where

import Freekick.Libsoccer.Stage
import Freekick.Libsoccer.Trophy
import Libaddutil.Primitives

data Tournament =
     Tournament { name          :: String,
--                participants  :: Int,           -- calculate sum of stages
                  repeatfreq    :: Int,           -- e.g. in weeks
                  teamlevel     :: Bool,          -- national teams or clubs
                  schedule      :: Schedule,
                  stages        :: [Stage],
                  trophy        :: Trophy
                }
     deriving (Eq, Show)

nullTournament :: Tournament
nullTournament = Tournament "" 0 False (nullDate, nullDate) [] nullTrophy

getTeamLevel :: Tournament -> Bool
getTeamLevel t = teamlevel t

averageTimeBetweenMatches :: Tournament -> Int
averageTimeBetweenMatches Tournament {schedule=sc,stages=st} = (scheduleLength sc) `div` totalMatchesPerClubInStages st

totalMatchesPerClub :: Tournament -> Int
totalMatchesPerClub Tournament {stages=st} = totalMatchesPerClubInStages st
