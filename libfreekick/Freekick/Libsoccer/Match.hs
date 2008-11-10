module Freekick.Libsoccer.Match
where

import IO
--import System.Process
--import qualified Data.Map
--import Data.Maybe
import Time
import Text.Printf
import Random

import Libaddutil.Primitives

--import Freekick.Libsoccer.Club
--import Freekick.Libsoccer.World
--import Freekick.Libsoccer.MatchInfo
--import Freekick.Libsoccer.Pitch
--import Freekick.Libsoccer.Formation
--import Freekick.Libsoccer.Lineup

data Match = 
    Match { homeclub    :: String
          , awayclub    :: String
          , place       :: String
          , playtime    :: DateTime
          , result      :: Result
          }
    deriving (Show, Eq)

data Result =
    Result { homegoals :: Int
           , awaygoals :: Int
           , homepen   :: Int
           , awaypen   :: Int
           , status    :: MatchState
           }
    deriving (Show, Eq)

data MatchState = NotPlayed
                 | Ongoing
                 | Played Winner
    deriving (Eq)

data Winner = NoWinner
            | Home
            | Away
            | Draw
    deriving (Eq)

instance Show MatchState where
    show NotPlayed         = "Not played yet"
    show Ongoing           = "Ongoing"
    show (Played NoWinner) = "Unknown"
    show (Played Home)     = "Played (home win)"
    show (Played Away)     = "Played (away win)"
    show (Played Draw)     = "Played (draw)"

newMatch :: String -> String -> String -> Date -> Time -> Match
newMatch c1 c2 s d t = Match c1 c2 s (d, t) nullResult

nullMatch :: Match
nullMatch = Match "" "" "" ((January, 1), (18, 00, 00)) nullResult

nullResult :: Result
nullResult = Result 0 0 0 0 NotPlayed

createResult :: Match -> IO Result
createResult m = do
--    let hc = homeclub m
--    let ac = homeclub m
--    let hp = map player (contracts hc)
--    let ap = map player (contracts ac)
    homeres <- getStdRandom (randomR (0, 3))
    awayres <- getStdRandom (randomR (0, 3))
    return (Result homeres awayres 0 0 (Played (getWinner homeres awayres 0 0)))

getWinner :: Int -> Int -> Int -> Int -> Winner
getWinner h a hp ap = 
    if h > a 
        then Home 
        else if h < a 
                 then Away 
                 else if hp > ap
                          then Home 
                          else if hp < ap 
                                   then Away 
                                   else Draw

createMatchDates :: Int -> Date -> Int -> [Date]
createMatchDates m sd d | m <= 0    = []
                        | otherwise = sd : createMatchDates (m - 1) (sd `addDaysToDate` d) d

createEmptyMatchesFromDates :: [Date] -> [Match]
createEmptyMatchesFromDates []     = []
createEmptyMatchesFromDates (d:ds) = (newemptymatch d) : (createEmptyMatchesFromDates ds)
    where newemptymatch n = setMatchDate nullMatch n

setMatchDate :: Match -> Date -> Match
setMatchDate m d = Match (homeclub m) (awayclub m) (place m) (d, (18, 00, 00)) (result m)

getMatchDate :: Match -> Date
getMatchDate m = getDate (playtime m)

printMatchData :: Match -> IO ()
printMatchData m = do putStrLn $ showMatchData m

showMatchData :: Match -> String
showMatchData m = printf "%-15s %30s %3s - %-3s %-30s" dd c1 r1 r2 c2
    where dd = showDate $ fst $ playtime m
          c1 = homeclub m
          c2 = awayclub m
          r1 = if status (result m) == Played Home || status (result m) == Played Away || status (result m) == Played Draw then show (homegoals (result m)) else " "
          r2 = if status (result m) == Played Home || status (result m) == Played Away || status (result m) == Played Draw then show (awaygoals (result m)) else " "

hasDate :: Date -> Match -> Bool
hasDate d m = getDate (playtime m) == d

hasDateTime :: DateTime -> Match -> Bool
hasDateTime d m = playtime m == d

matchHasDateTime :: DateTime -> (Match, Integer) -> Bool
matchHasDateTime d (m, _) = playtime m == d

playMatches :: [Match] -> IO [Match]
playMatches []     = return []
playMatches (m:ms) = do
    fstm <- playMatch m
    rest <- playMatches ms
    return $ fstm : rest

playMatch :: Match -> IO Match
playMatch m = do
    newres <- createResult m
    return (m{result = newres})

{-
-- TODO: "some" refactoring
playFullMatch :: Match -> PlayerDB -> ClubDB -> StadiumDB -> IO Match
playFullMatch m pdb cdb sdb = do
    let c1 = getClub (homeclub m) cdb
    let c2 = getClub (awayclub m) cdb
    let p1 = filterDBByKeys pdb (getPlayerIDs c1)
    let p2 = filterDBByKeys pdb (getPlayerIDs c2)
    let pc = testPitch       -- TODO: get the real pitch
    let pl1 = (Data.Map.elems p1)
    let pl2 = (Data.Map.elems p2)
    let contents = MatchInfo c1 c2 pl1 pl2 formation442 formation442 (fromJust $ lineup442fromList pl1) (fromJust $ lineup442fromList pl2) pc 
    bracket (openFile "/tmp/matchdata.md" WriteMode) hClose (\h -> hPutStrLn h (show contents))
    ph <- runProcess "./match" ["/tmp/matchdata.md"] (Just ".") Nothing Nothing Nothing Nothing
    exitcode <- waitForProcess ph
    putStrLn (show exitcode)
    newres <- createResult m
    return (m{result = newres})
-}

allMatchesPlayed :: [Match] -> Bool
allMatchesPlayed []     = True
allMatchesPlayed (m:ms) | length ms == 0 = matchPlayed m
                        | otherwise      = if matchPlayed m == False then False else allMatchesPlayed ms

matchPlayed :: Match -> Bool
matchPlayed m = if ((status (result m)) == NotPlayed) || ((status (result m)) == Ongoing) then False else True
