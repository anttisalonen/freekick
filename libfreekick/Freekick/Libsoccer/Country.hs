module Freekick.Libsoccer.Country
where

import Freekick.Libsoccer.Region
import Freekick.Libsoccer.LeagueSystem

data Country =
     Country { name         :: String,
               abbreviation :: String,
               leaguesystem :: LeagueSystem,
               regions      :: [Region],
               team         :: Integer,
               tournaments  :: [Integer]
             }
    deriving (Show, Eq)
