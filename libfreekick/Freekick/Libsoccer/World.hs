module Freekick.Libsoccer.World
where

import Freekick.Libsoccer.Player
import Freekick.Libsoccer.Club
import Freekick.Libsoccer.Stadium
import qualified Data.Map
import Data.Maybe

import Freekick.Libsoccer.Country
import Freekick.Libsoccer.Region

import Random

type PlayerDB  = Data.Map.Map Integer Player
type ClubDB    = Data.Map.Map String Club
type StadiumDB = Data.Map.Map String Stadium

getClub :: String -> ClubDB -> Club
getClub n cdb = Data.Map.findWithDefault nullClub n cdb

getRandomClubs :: ClubDB -> Int -> Int -> IO [Club]
getRandomClubs cdb num max_ = getRandomClubs' cdb num max_ []

getRandomClubs' :: ClubDB -> Int -> Int -> [Int] -> IO [Club]
getRandomClubs' cdb num max_ taken | num <= 0  = return []
                                  | otherwise = do
    (fstclub, newtaken) <- getRandomClub cdb taken max_
    restclubs <- getRandomClubs' cdb (num - 1) max_ newtaken
    return (fstclub : restclubs)

getRandomClub :: ClubDB -> [Int] -> Int -> IO (Club, [Int])
getRandomClub cdb taken max_ = do
    post <- getStdRandom (randomR (0, max_))
    if post `elem` taken 
        then do
             (actualclub, finaltaken) <- getRandomClub cdb (post : taken) max_ 
             return (actualclub, finaltaken)
        else return $ (snd (Data.Map.elemAt post cdb), (post : taken))

printPlayerNames :: Club -> PlayerDB -> IO ()
printPlayerNames (Club{contracts=c}) = printPlayerNamesFromContracts c

printPlayerNamesFromContracts :: [Contract] -> PlayerDB -> IO ()
printPlayerNamesFromContracts [] _ = return ()
printPlayerNamesFromContracts (c:cs) pdb = do
    printPlayerNameFromID (player c) pdb
    putStrLn ""
    printPlayerNamesFromContracts cs pdb

printPlayerNameFromID :: Integer -> PlayerDB -> IO ()
printPlayerNameFromID id_ pdb = putStr $ (Freekick.Libsoccer.Player.getFirstname pl) ++ " " ++ (Freekick.Libsoccer.Player.getLastname pl)
    where pl = fromJust $ Data.Map.lookup id_ pdb

printPlayerStatsFromClub :: Club -> PlayerDB -> IO ()
printPlayerStatsFromClub (Club{contracts=c}) = printPlayerStatsFromContracts c

getPlayerStatsFromClub :: Club -> PlayerDB -> [String]
getPlayerStatsFromClub c = getPlayerStatsFromContracts (contracts c)

getPlayerFromContract :: Contract -> PlayerDB -> Maybe Player
getPlayerFromContract c pdb = Data.Map.lookup (player c) pdb

getPlayersFromContracts :: [Contract] -> PlayerDB -> [Player]
getPlayersFromContracts []     _   = []
getPlayersFromContracts (c:cs) pdb = 
    if isNothing mpl
        then
            getPlayersFromContracts cs pdb 
        else
            ([fromJust mpl] ++ getPlayersFromContracts cs pdb)
    where mpl = getPlayerFromContract c pdb

-- TODO: refactoring
printPlayerStatsFromContracts :: [Contract] -> PlayerDB -> IO ()
printPlayerStatsFromContracts cs pdb = do
    printPlayersStats pls2
    where pls = getPlayersFromContracts cs pdb
          pls2 = sortByPosition pls

getPlayerStatsFromContracts :: [Contract] -> PlayerDB -> [String]
getPlayerStatsFromContracts cs pdb = getPlayersStats pls
    where pls = sortByPosition (getPlayersFromContracts cs pdb)

createPlayerDB :: [Player] -> PlayerDB
createPlayerDB ps = Data.Map.fromList (zip (map Freekick.Libsoccer.Player.getID ps) ps)

createClubDB :: [Club] -> ClubDB
createClubDB cs = Data.Map.fromList (zip (map Freekick.Libsoccer.Club.name cs) cs)

createStadiumDB :: Country -> StadiumDB
createStadiumDB c = Data.Map.fromList (zip (map Freekick.Libsoccer.Stadium.name ss) ss)
    where crs = regions c
          rs  = crs ++ concatMap allRegions crs
          ss  = concatMap stadiums rs

filterClubDBByNames :: ClubDB -> [String] -> ClubDB
filterClubDBByNames _   []     = Data.Map.empty
filterClubDBByNames cdb (n:ns) = if isNothing c then rest else Data.Map.insert n (fromJust c) rest
    where c    = Data.Map.lookup n cdb
          rest = filterClubDBByNames cdb ns 

filterDBByKeys :: (Ord k) => Data.Map.Map k a -> [k] -> Data.Map.Map k a
filterDBByKeys _  []     = Data.Map.empty
filterDBByKeys db (n:ns) = if isNothing c then rest else Data.Map.insert n (fromJust c) rest
    where c    = Data.Map.lookup n db
          rest = filterDBByKeys db ns
