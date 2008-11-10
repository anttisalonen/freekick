module Freekick.Libsoccer.Trophy
where

import System.IO

data Trophy = 
     Trophy { name          :: String,
              id            :: Int,
              image         :: FilePath,
              points        :: Int,
              bonus         :: Int
            }
     deriving (Eq, Show)

nullTrophy :: Trophy
nullTrophy = Trophy "" 0 "" 0 0
