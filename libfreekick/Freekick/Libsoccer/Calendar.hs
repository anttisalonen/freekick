module Freekick.Libsoccer.Calendar
where

import Libaddutil.Primitives
import Freekick.Libsoccer.TournamentInstance
import Freekick.Libsoccer.Stage
import Data.Maybe

data Calendar = Calendar { currentdate  :: Date
                         , tournaments  :: [TournamentInstance]
                         }
     deriving (Eq, Show)

newCalendar :: Date -> Calendar
newCalendar d = Calendar d []

addTournamentInstanceToCalendar :: Calendar -> TournamentInstance -> Calendar
addTournamentInstanceToCalendar Calendar{currentdate=d, tournaments=tl} t = Calendar d (t:tl)

showCalendar :: Calendar -> IO ()
showCalendar c = do
    putStrLn $ showDate $ currentdate c
    printDatesAndMatches $ tournaments c

increaseDate :: Calendar -> Calendar
increaseDate c = c{currentdate = (currentdate c) `addDaysToDate` 1}

playTodaysMatches :: Calendar -> IO Calendar
playTodaysMatches c = do
    let ts = tournaments c
    nn <- mapM (playMatchesOfTheDay (currentdate c)) ts
    let (newt, ccha) = unzip nn
    let fint = updateTournamentInstances newt (concat ccha)
    let newc = c{tournaments = fint}
    return newc

collectTournamentUpdates :: TournamentInstance -> [([String], StageTarget)]
collectTournamentUpdates t = updateTournamentInstance t

updateTournamentInstances :: [TournamentInstance] -> [([String], StageTarget)] -> [TournamentInstance]
updateTournamentInstances []     _                     = []
updateTournamentInstances ts     []                    = ts
updateTournamentInstances (t:ts) (c@(ns, (tt, _)):cs) = 
    if Freekick.Libsoccer.TournamentInstance.name t == tt 
        then (addClubsByNames t ns) : updateTournamentInstances ts (c:cs)
        else t : updateTournamentInstances ts (c:cs)

findTournamentInstance :: String -> [TournamentInstance] -> Maybe TournamentInstance
findTournamentInstance _ []     = Nothing
findTournamentInstance n (t:ts) = if Freekick.Libsoccer.TournamentInstance.name t == n then Just t else findTournamentInstance n ts
