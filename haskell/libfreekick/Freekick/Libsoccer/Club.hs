module Freekick.Libsoccer.Club
where

import System.IO
import Libaddutil.Colors
import Freekick.Libsoccer.Player
import Libaddutil.Person

data Club =
     Club   {   name        :: String,
                kits        :: [Kit],
                contracts   :: [Contract],
                logo        :: FilePath,
                formations  :: [String],
                country     :: String,
                region      :: String,
                stadium     :: String
            }
     deriving (Eq, Show, Read)

data Contract =
     Contract { player              :: Integer,
                contractlength      :: Int,
                salary              :: Int,
                bonus               :: Int
              }
     deriving (Eq, Show, Read)

data Kit =
     Kit {     jerseytype    :: Int,
               jerseycolors  :: [Color],
               shortscolor   :: [Color],
               sockscolor    :: [Color],
               jerseyimage   :: FilePath,
               shortsimage   :: FilePath
         }
     deriving (Eq, Show, Read)

nullClub :: Club
nullClub = Club "" [] [] "" [] "" "" ""

getClubNames :: [Club] -> String
getClubNames []     = []
getClubNames (c:cs) = getClubName c ++ getClubNames cs

getClubName :: Club -> String
getClubName c = Freekick.Libsoccer.Club.name c ++ "\n"

printPlayerStats :: Player -> IO ()
printPlayerStats pl = do
    let pls = skills pl
    let plp = personal pl
    let ppersonals = firstname plp ++ " " ++ lastname plp ++ "\t" ++ show (yearofbirth plp) ++ " " ++ (nationality plp)
    let ppos = positionToString (position pl)
    let pskills = show (stamina pls) ++ " " ++ show (speed pls) ++ " " ++ show (tackling pls) ++ " " ++ show (passing pls) ++ " " ++ show (accuracy pls) ++ " " ++ show (goalkeeping pls)
    putStrLn (ppersonals ++ "\t" ++ ppos ++ "\t" ++ pskills)

-- TODO: refactoring
getPlayerStats :: Player -> String
getPlayerStats pl = ppersonals ++ "\t" ++ ppos ++ "\t" ++ pskills
    where pskills = show (stamina pls) ++ " " ++ show (speed pls) ++ " " ++ show (tackling pls) ++ " " ++ show (passing pls) ++ " " ++ show (accuracy pls) ++ " " ++ show (goalkeeping pls)
          ppos = positionToString (position pl)
          ppersonals = firstname plp ++ " " ++ lastname plp ++ "\t" ++ show (yearofbirth plp) ++ " " ++ (nationality plp)
          pls = skills pl
          plp = personal pl

printPlayersStats :: [Player] -> IO ()
printPlayersStats []     = return ()
printPlayersStats (p:ps) = do
    printPlayerStats p
    printPlayersStats ps

getPlayersStats :: [Player] -> [String]
getPlayersStats = map getPlayerStats

getPlayerIDs :: Club -> [Integer]
getPlayerIDs c = map player (contracts c)
