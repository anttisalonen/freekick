module Freekick.Libsoccer.Organization
where

import Freekick.Libsoccer.Country
import Freekick.Libsoccer.Tournament

data Organization =
     Organization { name             :: String,
                    id               :: Integer,
                    suborganizations :: [Organization],
                    countries        :: [Country],
                    tournaments      :: [Tournament], 
                    hostorganization :: Integer
                  }
