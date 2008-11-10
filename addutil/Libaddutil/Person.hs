module Libaddutil.Person
where

import Libaddutil.Colors
import Libaddutil.Primitives

data Person =
     Person {   firstname   :: String,
                lastname    :: String,
                yearofbirth :: Int,
                birthdate   :: Date,
                skincolor   :: Color,
                haircolor   :: Color,
                eyecolor    :: Color,
                height      :: Int,
                nationality :: String
              }
     deriving (Show, Eq, Read)

getFirstname :: Person -> String
getFirstname p = firstname p

getLastname :: Person -> String
getLastname p = lastname p
