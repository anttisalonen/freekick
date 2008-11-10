module Freekick.Libsoccer.Region
where

import Freekick.Libsoccer.Stadium

data Region =
     Region { name         :: String,
              stadiums     :: [Stadium],
--            hostregion   :: Entity,
--            country      :: Entity,
--            clubs        :: [Entity],
              subregions   :: [Region]
            }
     deriving (Show, Eq)

allRegions :: Region -> [Region]
allRegions r = ss ++ concatMap allRegions ss
    where ss = subregions r
