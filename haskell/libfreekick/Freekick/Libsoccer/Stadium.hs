module Freekick.Libsoccer.Stadium
where

import System.IO
import Libaddutil.Primitives

data Stadium = 
     Stadium { name     :: String,
               lights   :: [PointLight],
               capacity :: Int,
               model    :: FilePath,
               pitch    :: String
             }
     deriving (Eq, Show)

nullStadium :: Stadium
nullStadium = Stadium "" [] 0 "" ""
