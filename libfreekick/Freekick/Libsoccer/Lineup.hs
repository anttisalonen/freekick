module Freekick.Libsoccer.Lineup
where

import Freekick.Libsoccer.Player

data Lineup =
    Lineup { goalkeeper  :: Integer
           , defenders   :: [Integer]
           , midfielders :: [Integer]
           , forwards    :: [Integer]
           , substitutes :: [Integer]
           }
    deriving (Eq, Show, Read)

validLineup :: Lineup -> Bool
validLineup l = length (defenders l) + length (midfielders l) + length (forwards l) == 10

-- |'lineup442fromList creates the simplest lineup from a list of players.
-- TODO: make it smarter.
lineup442fromList :: [Player] -> Maybe Lineup
lineup442fromList [] = Nothing
lineup442fromList p  = if not (null gl) && validLineup l then Just l else Nothing
    where l  = Lineup g ds ms fs ss
          gl = filter (\x -> (position x) == Goalkeeper) p
          g  =     idnum $ head   $ gl
          ds = map idnum $ take 4 $ filter (\x -> positionToInt (position x) == 1) p
          ms = map idnum $ take 4 $ filter (\x -> positionToInt (position x) == 2) p
          fs = map idnum $ take 2 $ filter (\x -> positionToInt (position x) == 3) p
          ss = []

lineupToList :: Lineup -> [Integer]
lineupToList l = [(goalkeeper l)] ++ (defenders l) ++ (midfielders l) ++ (forwards l) ++ (substitutes l)

playerIsAssigned :: Lineup -> Player -> Bool
playerIsAssigned l p = i == goalkeeper l || elem i (defenders l) || elem i (midfielders l) || elem i (forwards l)
    where i = idnum p

playerIsSubstitute :: Lineup -> Player -> Bool
playerIsSubstitute l p = elem (idnum p) (substitutes l)
