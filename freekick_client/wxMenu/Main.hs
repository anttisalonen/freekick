module Main
where

import System
import System.IO
import System.Directory
import System.Process
import Time
import Data.Maybe
import qualified Data.Map
import Random
import Control.Concurrent

import Graphics.UI.WX

import Libaddutil.XMLParse as XMLParse
import Libaddutil.ListUtils as ListUtils
import Libaddutil.Primitives as Primitives       -- date
import Libaddutil.Person as Person

import Freekick.Libsoccer.Stage as Stage
import Freekick.Libsoccer.Tournament as Tournament
import Freekick.Libsoccer.Database as Database
import Freekick.Libsoccer.Stadium as Stadium
import Freekick.Libsoccer.Match as Match
import Freekick.Libsoccer.Trophy as Trophy
import Freekick.Libsoccer.TournamentInstance as TournamentInstance
import Freekick.Libsoccer.Calendar as Calendar

import Freekick.Libsoccer.Player as Player
import Freekick.Libsoccer.Club as Club
import Freekick.Libsoccer.Country as Country
import Freekick.Libsoccer.Region as Region
import Freekick.Libsoccer.MatchInfo as MatchInfo

import Freekick.Libsoccer.World as World

inputClub :: ClubDB -> IO Club
inputClub db = do
    putStrLn "Please enter a club name: "
    clname <- getLine
    let maybeclub = Data.Map.lookup clname db
    if isNothing maybeclub 
        then do putStrLn "Club not found in the database"
                inputClub db
        else do putStrLn "Club found"
                return $ fromJust maybeclub

loadDefaultPlayers :: [Char] -> IO PlayerDB
loadDefaultPlayers p = do
    players1 <- parsePlayerXML (p ++ "/DB/Players/PlEng1.xml")
    players2 <- parsePlayerXML (p ++ "/DB/Players/PlEng2.xml")
    players3 <- parsePlayerXML (p ++ "/DB/Players/PlEng3.xml")
    players4 <- parsePlayerXML (p ++ "/DB/Players/PlEng4.xml")
    players5 <- parsePlayerXML (p ++ "/DB/Players/PlEng5.xml")
    let playerDB = createPlayerDB (players1 ++ players2 ++ players3 ++ players4 ++ players5)
    return playerDB

main :: IO ()
main = start gui

gui :: IO ()
gui = do
    f  <- frame [text := "Freekick menu"]
    sb <- staticText f [text := "Loading data"]
    st <- staticText f [text := "Main menu"]
    bc <- button     f [text := "DIY Cup"]
    bf <- button     f [text := "Friendly game"]
    ex <- button     f [text := "Exit", on command := close f]
    set f [layout := row 0 [widget st, widget bc, widget bf, widget sb, widget ex], clientSize := sz 400 300]
    dirprefix <- getAppUserDataDirectory "freekick"
    stdpitches <- parsePitchXML (dirprefix ++ "/DB/Pitches.xml")
    england <- parseCountryXML (dirprefix ++ "/DB/Countries/England.xml")
    playerDB <- loadDefaultPlayers dirprefix
    allclubs <- parseClubXML (dirprefix ++ "/DB/Countries/English.xml")
    let clubDB = createClubDB (allclubs)
    let stadiumDB = createStadiumDB (head england)
    set sb [text := "Finished loading"]
    set bc [on command := guidiycup playerDB clubDB stadiumDB]
    set bf [on command := guifriendly playerDB clubDB stadiumDB]
    return ()

guifriendly :: PlayerDB -> ClubDB -> StadiumDB -> IO ()
guifriendly playerDB clubDB stadiumDB = do
    f <- frame [text := "Friendly game"]
    let allclubnames = Data.Map.keys clubDB
    c1 <- comboBox f [items := allclubnames]
    c2 <- comboBox f [items := allclubnames]
    bt <- button   f [text  := "Continue", on command := preparelineups playerDB clubDB stadiumDB c1 c2]
    ex <- button   f [text  := "Exit", on command := close f]
    set f [layout := column 0 [widget c1, widget c2, widget bt, widget ex]]
    return ()

{-          Inferred type: preparelineups :: forall a a1.
                                              (Selection a1, Items a1 a, Ord a) =>
                                              PlayerDB
                                              -> Data.Map.Map a Club
                                              -> StadiumDB
                                              -> a1
                                              -> a1
                                              -> IO () -}
preparelineups pdb cdb sdb w1 w2 = do
    mcs <- mapM (getClubFromBox cdb) [w1,w2]
    let cs = catMaybes mcs
    if length cs == 2
      then guilineups pdb sdb (head cs) (last cs)
      else return ()

{-             Inferred type: getClubFromBox :: forall w a a1 (m :: * -> *).
                                              (Ord a, Monad m, Items w a, Selection w) =>
                                              Data.Map.Map a a1 -> w -> IO (m a1) -}
getClubFromBox cdb w = do
    i <- get w selection
    s <- get w (item i)
    return $ Data.Map.lookup s cdb

guilineups :: PlayerDB -> StadiumDB -> Club -> Club -> IO ()
guilineups pdb sdb c1 c2 = do
    f <- frame [text := "Lineups"]
    let pd1 = getPlayerStatsFromClub c1 pdb
    let pd2 = getPlayerStatsFromClub c2 pdb
    let homestad = fromJust $ Data.Map.lookup (stadium c1) sdb
    let friendlygame = newMatchInfo pdb c1 c2 homestad ((June, 23), (18, 0, 0))
    pl1 <- multiListBox f [ items := pd1]
    pl2 <- multiListBox f [ items := pd2]
    st <- button f [text := "Start", on command := guimatch f friendlygame]
    ex <- button f [text := "Exit", on command := close f]
    set f [layout := column 0 [widget pl1, widget pl2, widget st, widget ex]]
    return ()

{-
    putStrLn ""
    putStrLn "First club players are: "
    printPlayerStatsFromClub club1 playerDB
    putStrLn ""
    putStrLn "Second club players are: "
    printPlayerStatsFromClub club2 playerDB
    frresult <- playFullMatch friendlygame playerDB clubDB stadiumDB
    putStrLn ("Venue: " ++ (show homestad))
    putStrLn ("Friendly game result: " ++ (show frresult))
-}

guimatch f m = do
    startmatch m
    close f

startmatch :: MatchInfo -> IO ()
startmatch m = do
    bracket (openFile "/tmp/matchdata.md" WriteMode) hClose (\h -> hPutStrLn h (show m))
    sv <- runProcess "./physics" ["/tmp/matchdata.md"] (Just ".") Nothing Nothing Nothing Nothing
    threadDelay 500000
    ai <- runProcess "./ai"    [] Nothing Nothing Nothing Nothing Nothing
    cl <- runProcess "./fksdl" [] Nothing Nothing Nothing Nothing Nothing
    exitcode <- waitForProcess cl
    terminateProcess ai
    threadDelay 100000
    terminateProcess sv
    putStrLn (show exitcode)
    return ()

guidiycup :: PlayerDB -> ClubDB -> StadiumDB -> IO ()
guidiycup playerDB clubDB stadiumDB = do
    let maxclubs = Data.Map.size clubDB
    putStrLn ("How many clubs should participate? (out of " ++ (show maxclubs) ++ " clubs)")
    numclubsread <- getLine
    let tempnumclubs = nextPowerOfTwo (read numclubsread)
    let numclubs = if tempnumclubs > maxclubs then nextPowerOfTwo (maxclubs `div` 2) else tempnumclubs
    putStrLn $ "Number of clubs: " ++ (show numclubs)
    cupclubs <- getRandomClubs clubDB numclubs (maxclubs - 1)
    putStrLn "Club names: "
    putStrLn $ getClubNames cupclubs
    getLine
    let cupname = "DIY Cup"
    let cupstages = createStdStagesFromClubNumber cupname (length cupclubs)
    let startdate = (June, 1)
    let enddate = (July, 1)
    let thiscup = Tournament cupname 0 False (startdate, enddate) cupstages nullTrophy
    let emptydiycup = createTournamentInstance thiscup
    let thisdiycup = addClubs emptydiycup cupclubs
    let emptycalendar = newCalendar (May, 28)
    let partclubs = Data.Map.fromList (zip (map Club.name cupclubs) cupclubs)
    runGame (addTournamentInstanceToCalendar emptycalendar thisdiycup) partclubs

diycup :: PlayerDB -> ClubDB -> StadiumDB -> IO ()
diycup playerDB clubDB stadiumDB = do
    let maxclubs = Data.Map.size clubDB
    putStrLn ("How many clubs should participate? (out of " ++ (show maxclubs) ++ " clubs)")
    numclubsread <- getLine
    let tempnumclubs = nextPowerOfTwo (read numclubsread)
    let numclubs = if tempnumclubs > maxclubs then nextPowerOfTwo (maxclubs `div` 2) else tempnumclubs
    putStrLn $ "Number of clubs: " ++ (show numclubs)
    cupclubs <- getRandomClubs clubDB numclubs (maxclubs - 1)
    putStrLn "Club names: "
    putStrLn $ getClubNames cupclubs
    getLine
    let cupname = "DIY Cup"
    let cupstages = createStdStagesFromClubNumber cupname (length cupclubs)
    let startdate = (June, 1)
    let enddate = (July, 1)
    let thiscup = Tournament cupname 0 False (startdate, enddate) cupstages nullTrophy
    let emptydiycup = createTournamentInstance thiscup
--    putStrLn $ show thiscup
--    getLine
--    putStrLn $ show emptydiycup
--    getLine
    let thisdiycup = addClubs emptydiycup cupclubs
    let emptycalendar = newCalendar (May, 28)
    let partclubs = Data.Map.fromList (zip (map Club.name cupclubs) cupclubs)
    runGame (addTournamentInstanceToCalendar emptycalendar thisdiycup) partclubs

runGame :: Calendar -> ClubDB -> IO ()
runGame c cdb = do
--    putStrLn (show c)
    showCalendar c
    getLine
    newc <- playTodaysMatches c
    putStrLn $ printTournamentInstanceInfo $ head $ Calendar.tournaments $ newc
    runGame (increaseDate newc) cdb


