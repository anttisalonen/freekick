module Freekick.Libsoccer.Scenario
where

import Libaddutil.Primitives

data Scenario = 
    Scenario { name           :: String,
               id             :: Integer,
               ballstartloc   :: Point,
               ballendloc     :: Point,
               playerlocs     :: [Point]
             }
    deriving (Show, Read, Eq)
