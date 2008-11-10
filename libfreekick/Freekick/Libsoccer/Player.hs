module Freekick.Libsoccer.Player
where

import Libaddutil.Person

import Freekick.Libsoccer.Common

data Player =
     Player {   idnum       :: Integer,
                personal    :: Person,
                personality :: PlayerPersonality,
                skills      :: PlayerSkills,
                position    :: PlayerPosition,
                club        :: String
            }
     deriving (Show, Eq, Read)

data PlayerPersonality =
     PlayerPersonality { activeplaying       :: Int,
                         risktaking          :: Int,
                         offensiveness       :: Int,
                         aggressiveness      :: Int,
                         consistency         :: Int,
                         creativity          :: Int,
                         experience          :: Int
                       }
     deriving (Show, Eq, Read)

data PlayerSkills = 
     PlayerSkills { stamina                  :: Int,
                    dexterity                :: Int,
                    speed                    :: Int,
                    tackling                 :: Int,
                    passing                  :: Int,
                    shooting                 :: Int,
                    control                  :: Int,
                    accuracy                 :: Int,
                    goalkeeping              :: Int,
                    heading                  :: Int
                  }
     deriving (Show, Eq, Read)

data PlayerPosition = Goalkeeper
                    | Defender PitchSide Bool
                    | Midfielder PitchSide Bool
                    | Forward PitchSide Bool
     deriving (Show, Eq, Ord, Read)

getID :: Player -> Integer
getID p = idnum p

getFirstname :: Player -> String
getFirstname p = Libaddutil.Person.getFirstname $ personal p

getLastname :: Player -> String
getLastname p = Libaddutil.Person.getLastname $ personal p

positionLessThan :: [Player] -> PlayerPosition -> [Player]
positionLessThan []     _  = []
positionLessThan (p:ps) pp = if position p < pp then (p : (positionLessThan ps pp)) else positionLessThan ps pp

positionNotLessThan :: [Player] -> PlayerPosition -> [Player]
positionNotLessThan []     _  = []
positionNotLessThan (p:ps) pp = if position p >= pp then (p : (positionNotLessThan ps pp)) else positionNotLessThan ps pp

sortByPosition :: [Player] -> [Player]
sortByPosition []     = []
sortByPosition (p:ps) = sortByPosition (ps `positionLessThan` (position p)) ++ [p] ++ sortByPosition (ps `positionNotLessThan` (position p))

positionToInt :: PlayerPosition -> Int
positionToInt p = case p of
                    Goalkeeper      -> 0
                    Defender   _ _  -> 1
                    Midfielder _ _  -> 2
                    Forward    _ _  -> 3

positionToString :: PlayerPosition -> String
positionToString p = case p of
                       Goalkeeper         -> "Goalkeeper"
                       Defender       _ w -> case w of True  -> "Full back"
                                                       False -> "Centre back"
                       Midfielder     _ w -> case w of True  -> "Side midfielder"
                                                       False -> "Centre midfielder"
                       Forward        _ _ -> "Forward"

newPlayerPosition :: Int -> Bool -> Bool -> PlayerPosition
newPlayerPosition p l w = let s = if l then LeftSide else RightSide
                          in case p of
                               0 -> Goalkeeper
                               1 -> Defender s w
                               2 -> Midfielder s w
                               _ -> Forward s w
