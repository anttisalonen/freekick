module Main
where

import System.IO
import Freekick.Libsoccer.Stage as Stage
import Freekick.Libsoccer.Tournament as Tournament
import Freekick.Libsoccer.Database as Database
import Freekick.Libsoccer.Trophy as Trophy
import Freekick.Libsoccer.TournamentInstance as TournamentInstance
import Freekick.Libsoccer.Calendar as Calendar
import Libaddutil.ListUtils as ListUtils

import Time

import Freekick.Libsoccer.Club as Club
import Data.Maybe
import qualified Data.Map
import Freekick.Libsoccer.World as World

import System.Directory

inputClub :: ClubDB -> IO Club
inputClub db = do
    putStrLn "Please enter a club name: "
    cname <- getLine
    let maybeclub = Data.Map.lookup cname db
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
main = do
    dirprefix <- getAppUserDataDirectory "freekick"
    stdpitches <- parsePitchXML (dirprefix ++ "/DB/Pitches.xml")
    england <- parseCountryXML (dirprefix ++ "/DB/Countries/England.xml")
    playerDB <- loadDefaultPlayers dirprefix
    aclubs <- parseClubXML (dirprefix ++ "/DB/Countries/English.xml")
    let clubDB = createClubDB (aclubs)
    let stadiumDB = createStadiumDB (head england)

    putStrLn "Input 'f' for a friendly or anything else for a cup. "
    mode <- getLine
    if length mode > 0 && head mode == 'f' then friendly playerDB clubDB stadiumDB else diycup playerDB clubDB stadiumDB

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
    let aclubs = Data.Map.fromList (zip (map Club.name cupclubs) cupclubs)
    runGame (addTournamentInstanceToCalendar emptycalendar thisdiycup) aclubs

runGame :: Calendar -> ClubDB -> IO ()
runGame c cdb = do
--    putStrLn (show c)
    showCalendar c
    getLine
    newc <- playTodaysMatches c
    putStrLn $ printTournamentInstanceInfo $ head $ Calendar.tournaments $ newc
    runGame (increaseDate newc) cdb

friendly :: PlayerDB -> ClubDB -> StadiumDB -> IO ()
friendly playerDB clubDB stadiumDB = do
    putStrLn "First club: "
    club1 <- inputClub clubDB
    putStrLn "Second club: "
    club2 <- inputClub clubDB
    putStrLn ""
    putStrLn "First club players are: "
    printPlayerStatsFromClub club1 playerDB
    putStrLn ""
    putStrLn "Second club players are: "
    printPlayerStatsFromClub club2 playerDB
    let stadname = stadium club1
    let homestad = fromJust $ Data.Map.lookup stadname stadiumDB
--    let friendlygame = newMatch (Club.name club1) (Club.name club2) (Stadium.name homestad) (June, 23) (18, 0, 0)
--    result <- playFullMatch friendlygame playerDB clubDB stadiumDB
    putStrLn ("Venue: " ++ (show homestad))
--    putStrLn ("Friendly game result: " ++ (show result))
--    let field1 = chooseField stdFormation442 club1

