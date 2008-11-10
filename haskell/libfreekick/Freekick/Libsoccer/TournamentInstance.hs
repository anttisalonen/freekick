module Freekick.Libsoccer.TournamentInstance
where

import Freekick.Libsoccer.Tournament
import Freekick.Libsoccer.Stage
import Freekick.Libsoccer.Club
import Freekick.Libsoccer.Match
import Libaddutil.Primitives
import Data.Maybe
import Data.List

data TournamentInstance = 
     TournamentInstance { name           :: String
                        , stageinstances :: [StageInstance]
                        }
    deriving (Eq, Show)

data StageInstance      = 
     StageInstance { stage      :: Stage
                   , clubs      :: [String]
                   , rounds     :: [Round]
                   , finished   :: Bool
                   }
    deriving (Eq, Show)

type Round = [Match]

createTournamentInstance :: Tournament -> TournamentInstance
createTournamentInstance t = TournamentInstance (Freekick.Libsoccer.Tournament.name t) ss
    where ss = createStageInstances t

createStageInstances :: Tournament -> [StageInstance]
createStageInstances t = createStageInstancesFromStages (stages t) (beginDate (schedule t)) datediff
    where datediff   = averageTimeBetweenMatches t

createStageInstancesFromStages :: [Stage] -> Date -> Int -> [StageInstance]
createStageInstancesFromStages [] _ _ = []
createStageInstancesFromStages (s:ss) startdate datediff = thisstageinstance : (createStageInstancesFromStages ss nextdate datediff)
    where thisstageinstance = createStageInstance s startdate datediff
          nextdate          = if isNothing lmd then startdate `addDaysToDate` datediff else fromJust (lmd) `addDaysToDate` (2 * datediff)
          lmd               = lastMatchDate thisstageinstance

createStageInstance :: Stage -> Date -> Int -> StageInstance
createStageInstance s startdate datediff = StageInstance s [] rs False
    where rs         = take ((participantnum (stagesetup s)) `div` 2) (repeat (createEmptyMatchesFromDates matchdates))
          matchdates = createMatchDates (countMatchesPerClubInStage s) startdate datediff

lastMatchDate :: StageInstance -> Maybe Date
lastMatchDate s | length (Freekick.Libsoccer.TournamentInstance.rounds s) == 0 = Nothing
                | otherwise                                 = Just $ fst (playtime lm)
    where lm = head (last (Freekick.Libsoccer.TournamentInstance.rounds s))

addClubs :: TournamentInstance -> [Club] -> TournamentInstance
addClubs t cs | length cs == 0 = t
addClubs t cs | otherwise      = TournamentInstance (Freekick.Libsoccer.TournamentInstance.name t) assignedss
    where assignedss  = assignClubsToStages newss
          newss       = addClubs' oldss clubnames
          oldss       = stageinstances t
          clubnames   = map Freekick.Libsoccer.Club.name cs

addClubsByNames :: TournamentInstance -> [String] -> TournamentInstance
addClubsByNames t ns | length ns == 0 = t
addClubsByNames t ns | otherwise      = t{stageinstances = assignedss}
    where assignedss = assignClubsToStages newss
          newss      = addClubs' (stageinstances t) ns

addClubs' :: [StageInstance] -> [String] -> [StageInstance]
addClubs' []     _                   = []
addClubs' s      []                  = s
addClubs' (s:ss) cs = if neededclubs <= 0 then s : (addClubs' ss cs) else added : (addClubs' ss (drop neededclubs cs))
          where neededclubs = participantnum (stagesetup (stage s)) - (length (clubs s))
                added       = addClubs'' s (take neededclubs cs)

addClubs'' :: StageInstance -> [String] -> StageInstance
addClubs'' olds [] = olds
addClubs'' olds cs = olds{clubs=clubs olds ++ cs}

freeSlots :: StageInstance -> Int
freeSlots s = freeSlots' (head (Freekick.Libsoccer.TournamentInstance.rounds s))

freeSlots' :: [Match] -> Int
freeSlots' []     = 0
freeSlots' (m:ms) = ht + at + freeSlots' ms
    where ht = if homeclub m == "" then 1 else 0
          at = if awayclub m == "" then 1 else 0

assignClubsToTournamentMatches :: TournamentInstance -> TournamentInstance
assignClubsToTournamentMatches t = t{Freekick.Libsoccer.TournamentInstance.stageinstances = newst}
    where newst = assignClubsToStages (stageinstances t)

assignClubsToStages :: [StageInstance] -> [StageInstance]
assignClubsToStages []     = []
assignClubsToStages (s:ss) = if freeSlots s >= length (clubs s)
                                 then (assignClubsToStageMatches s (clubs s) : ss)
                                 else (assignClubsToStageMatches s (clubs s) : assignClubsToStages ss)

assignClubsToStageMatches :: StageInstance -> [String] -> StageInstance
assignClubsToStageMatches s cs = s{Freekick.Libsoccer.TournamentInstance.rounds = newrounds}
    where newrounds = assignClubsToStageMatches' (Freekick.Libsoccer.TournamentInstance.rounds s) cs

assignClubsToStageMatches' :: [Round] -> [String] -> [Round]
assignClubsToStageMatches' []     _  = []
assignClubsToStageMatches' r      [] = r
assignClubsToStageMatches' (r:rs) (c:cs) | length cs == 0 = (r:rs)
                                         | otherwise      = (assignClubsToRound r c (head cs)) : (assignClubsToStageMatches' rs (tail cs))

assignClubsToRound :: Round -> String -> String -> Round
assignClubsToRound []     _  _  = []
assignClubsToRound (m:ms) c1 c2 = (assignClubsToMatch m c1 c2) : (assignClubsToRound ms c1 c2)

assignClubsToMatch :: Match -> String -> String -> Match
assignClubsToMatch m c1 c2 = m{homeclub = c1, awayclub = c2}

printDatesAndMatches :: [TournamentInstance] -> IO ()
printDatesAndMatches [] = return ()
printDatesAndMatches ts = do
    let allmatches = getAllMatches ts
    let sortedmatches = sortMatchesByDate allmatches
    printRoundData $ sortedmatches

getAllMatches :: [TournamentInstance] -> [Match]
getAllMatches []     = []
getAllMatches (t:ts) = (concat (map getAllMatches' (stageinstances t))) ++ getAllMatches ts

getAllMatches' :: StageInstance -> [Match]
getAllMatches' s = concat $ rounds s

sortMatchesByDate :: [Match] -> [Match]
sortMatchesByDate [] = []
sortMatchesByDate (m:ms) = sortMatchesByDate (filter (`earlierMatch` m) ms) ++ 
            [m] ++ 
            sortMatchesByDate (filter (`laterOrSamedateMatch` m) ms)

earlierMatch :: Match -> Match -> Bool
earlierMatch m1 m2 = if (getMatchDate m1) `earlierDate` (getMatchDate m2) then True else False

laterOrSamedateMatch :: Match -> Match -> Bool
laterOrSamedateMatch m1 m2 = not (earlierMatch m1 m2)

printDatesAndMatchesFromTI :: TournamentInstance -> IO ()
printDatesAndMatchesFromTI t = printDatesAndMatchesFromSIs $ stageinstances t

printDatesAndMatchesFromSIs :: [StageInstance] -> IO ()
printDatesAndMatchesFromSIs [] = return ()
printDatesAndMatchesFromSIs (s:ss) = do
    printDatesAndMatchesFromSI s
    printDatesAndMatchesFromSIs ss

printDatesAndMatchesFromSI :: StageInstance -> IO ()
printDatesAndMatchesFromSI s = printRoundData $ concat $ rounds s

printRoundData :: [Match] -> IO ()
printRoundData []     = return ()
printRoundData (m:ms) = do
    printMatchData m
    printRoundData ms

playMatchesOfTheDay :: Date -> TournamentInstance -> IO (TournamentInstance, [([String], StageTarget)])
playMatchesOfTheDay d t = do
    sc <- mapM (playMatchesOfTheDay'' d) (stageinstances t)
    let (ss, ch) = unzip sc
    let newt = t{stageinstances = ss}
    return (newt, concat ch)

playMatchesOfTheDay'' :: Date -> StageInstance -> IO ((StageInstance, [([String], StageTarget)]))
playMatchesOfTheDay'' d s = do
    if finished s == True 
        then return (s, [])
        else do
            newr <- mapM (playRounds' d) (rounds s)
            let fin = allMatchesPlayed (concat newr)
            let news = s{rounds = newr, finished = fin}
            let ch = if fin == False then [] else updateStageInstance news
            return (news, ch)

playRounds' :: Date -> [Match] -> IO [Match]
playRounds' _ [] = return []
playRounds' d ms = do
    let (pls, rest) = partition (hasDate d) ms
    newps <- playMatches pls
    return (newps ++ rest)

setFinishedFlag :: StageInstance -> StageInstance
setFinishedFlag s = if stageInstanceFinished s then s{finished = True} else s{finished = False}

updateTournamentInstance :: TournamentInstance -> [([String], StageTarget)]
updateTournamentInstance t = concat $ map updateStageInstance (stageinstances t)

updateStageInstance :: StageInstance -> [([String], StageTarget)]
updateStageInstance s = if (finished s) then listMovedClubs s else []

listMovedClubs :: StageInstance -> [([String], StageTarget)]
listMovedClubs s = listMovedClubs' (stage s) (rounds s)

listMovedClubs' :: Stage -> [Round] -> [([String], StageTarget)]
listMovedClubs' _                                                 [] = []
listMovedClubs' League{promotions=pr, relegations=rl}             rs = []   -- TODO
listMovedClubs' Knockout{promotiontarget=pt, relegationtarget=rt} rs = [(kowinners, pt), (kolosers, rt)]
    where (kowinners, kolosers) = unzip $ map getKOWinners rs

getKOWinners :: [Match] -> (String, String)
getKOWinners ms = if sum1 > sum2 then (ht, at) else (at, ht)
    where sum1 = sum (map (homegoals . result) ms)
          sum2 = sum (map (awaygoals . result) ms)
          ht = homeclub $ head ms
          at = awayclub $ head ms

stageInstanceFinished :: StageInstance -> Bool
stageInstanceFinished s = if freeSlots s > 0 then False else allMatchesPlayed (concat (rounds s))

printTournamentInstanceInfo :: TournamentInstance -> String
printTournamentInstanceInfo t = Freekick.Libsoccer.TournamentInstance.name t ++ "\n" ++ (intercalate "\n" (map printStageInstanceInfo (stageinstances t)))

printStageInstanceInfo :: StageInstance -> String
printStageInstanceInfo s = printStageInfo (stage s) ++ "\nClubs\n" ++ (intercalate "\n" (clubs s)) ++ "\n"
