module Main
where

import System.IO
import System
import IO hiding (bracket, catch)
import qualified Data.Map
import Data.Maybe
import Control.Concurrent    
import Control.Concurrent.STM
import Control.Exception
import Prelude hiding (catch)
import Char
import Data.Maybe
import Control.Monad.State
import System.Time
import Debug.Trace

import Libaddutil.Vector
import Libaddutil.Entity
import Libaddutil.STMServer
import Libaddutil.STMUtils
import Libaddutil.Primitives

import Freekick.Libsoccer.MatchInfo
import Freekick.Libsoccer.Player

import Freekick.Libmatch.Control
import Freekick.Libmatch.MatchStatus
import Freekick.Libmatch.PlayerInfo
import Freekick.Libmatch.Rules
import Freekick.Libmatch.SoccerPhysics

import Console

port :: Int
port = 32105

type BroadcastCallback = (IO String)

data ClientType = InvalidClient
                | Human [Integer]
                | AI [Integer]

-- |'main' is the entry point, it starts the thread 'startBroadcast' and spawns
-- TChan 'controlChannel' which has all the controls valid control events from
-- the clients as recorded by 'controlEventHandler' as well as the 
-- TVar matchBox that holds the current match status
main :: IO ()
main = do
    cmd <- getArgs
    contents_text <- readStrict (head cmd)
    let contents :: MatchInfo
        contents = read contents_text
    controlChannel <- atomically $ newTChan
    let m = createMatchStatusFromMatchInfo contents
    matchBox <- atomically $ newTVar m
    clientpool <- startServer port (clientHandler contents matchBox) (dataHandler controlChannel matchBox)
    tnow <- getClockTime
    forkIO $ startBroadcast clientpool 15000 matchBox controlChannel tnow False
    doInterpreter clientpool matchBox startConsoleStatus
--    putStrLn "Finished serving!"

startBroadcast :: TVar [Client]         -- ^ client pool
               -> Integer               -- ^ broadcast repeat interval in microseconds
               -> TVar MatchStatus      -- ^ match status
               -> TChan String          -- ^ control channel
               -> ClockTime             -- ^ time of last broadcast
               -> Bool                  -- ^ paused (no updates to match status)
               -> IO ()                 -- ^ return
startBroadcast cvar interval mb cc tthen pause | seq cvar $ seq interval $ seq mb $ seq cc $ seq tthen $ seq pause $ False = undefined
startBroadcast cvar interval mb cc tthen pause = do
    m <- atomically $ readTVar mb
    cs <- getAllFromTChan cc
--    let pcs = concat cs
--    when (not (null pcs)) $ putStrLn $ (show pcs)
    tnow <- getClockTime
    let addt = diffClockTimes tnow tthen
    let mtime = addTimeToMatchStatus m addt
    let addi = if tdPicosec addt > 0 then tdPicosec addt `div` 1000000 else (tdPicosec addt + 1000000000000) `div` 1000000
    let (bl, newm) = if not pause 
                       then updateMatchStatus cs ((fromInteger (interval + addi)) / 1000000.0) mtime
                       else ("(A 1)", m)
    let obp = ballplay m
    let nbp = ballplay newm
--    putStrLn $ show (ball newm)
    when (obp /= nbp) (putStrLn ("New ball status: " ++ (show nbp)))
    cls <- atomically $ readTVar cvar
    if (not (null cls)) 
        then do
            dcs <- broadcast cvar bl                          -- <- mem leak <-
            let mdr = foldl removeClientFromMatchStatus newm dcs
            when (not (null dcs)) $ putStrLn ("Clients removed: " ++ (show $ length dcs))
            atomically $ writeTVar mb mdr
        else do
            atomically $ writeTVar mb newm
    threadDelay $ fromInteger interval
    startBroadcast cvar interval mb cc tnow pause

updateMatchStatus :: [String]                 -- ^ event list without brackets
                  -> Float                    -- ^ time delta
                  -> MatchStatus              -- ^ old match status
                  -> (String, MatchStatus)    -- ^ (broadcasting string, new match status)
updateMatchStatus cs i | seq cs $ seq i $ False = undefined
updateMatchStatus cs i = runState (do
    updatePlayersWithControls cs
    updateBallOfPlay
    updateBallState i
    updateCollidingPlayersAndBall i
    b <- updateGoals
    return b)

updateGoals :: State MatchStatus String
updateGoals = do
    m  <- get
    let homegls = if lineCrossed (ballentity (ball m)) (pitch m) == Goal Up   then (home m) + 1 else home m
    let awaygls = if lineCrossed (ballentity (ball m)) (pitch m) == Goal Down then (away m) + 1 else away m
    let m' = m{home=homegls,away=awaygls}
    put m'
    let st = ballPlayToEvent (ballplay m')
    let ps = concat $ map playerPositionToString (Data.Map.elems (homeplayers m') ++ Data.Map.elems (awayplayers m'))
    return (ps ++ (ballPositionToString $ ball m) ++ st ++ "\n")

updateCollidingPlayersAndBall :: Float -> State MatchStatus ()
updateCollidingPlayersAndBall i = do
    m  <- get
    let homep = Data.Map.map (updatePlayer i) (homeplayers m)
    let awayp = Data.Map.map (updatePlayer i) (awayplayers m)
    let allp = Data.Map.elems homep ++ Data.Map.elems awayp
    let h = Data.Map.map (playerHandleCollisionWithOthers allp) homep
    let a = Data.Map.map (playerHandleCollisionWithOthers allp) awayp
    let (ncb, ndir) = foldl ballCollideWith ((ball m), getControllingDirection m) allp
    let nbp = if not (ballInPlay (ballplay m)) || ndir == getControllingDirection m then (ballplay m) else changeBallOwner ndir (ballplay m)
    let m' = m{homeplayers=h,awayplayers=a,ball=ncb,ballplay=nbp}
    put m'

updateBallState :: Float -> State MatchStatus ()
updateBallState i = do
    m  <- get
    let b = case (ballplay m) of
               BallOut Kickoff    _ _ True -> setBallLocation (getCentreSpot (pitch m)) (ball m)
               BallOut PreKickoff _ _ _    -> setBallLocation (getCentreSpot (pitch m)) (ball m)
               BallOut _          _ v _    -> setBallLocation v (ball m)
               _                           -> updateBall i (ball m)
    let m' = m{ball=b}
    put m'

addTimeToMatchStatus :: MatchStatus -> TimeDiff -> MatchStatus
addTimeToMatchStatus m t = m{matchtime=newtime}
    where newtime = addPreciseTime (timeDiffToPreciseTime t) (matchtime m)

updateBallOfPlay :: State MatchStatus ()
updateBallOfPlay = do
    m  <- get
    let oldmatchst    = ballplay m
    let p             = pitch m
    let (bax,_,baz)   = acceleration $ ballentity $ ball m
    let ballkicked    = vectorLength (velocity (ballentity (ball m))) > 0 || vectorLength (bax,0,baz) > 0
    let nb = if finalWhistle m 
               then BallOut HalfFullTime Up (0,0,0) True
               else case oldmatchst of
                      BallIn bo -> case lineCrossed (ballentity (ball m)) (pitch m) of
                                     None    -> BallIn bo
                                     Side s  -> BallOut Throwin (other bo) (getSide s (pitch m),0,(getZ . location . ballentity . ball) m) True
                                     Goal bn -> BallOut Kickoff (other bn) (getCentreSpot p) True
                                     End a s -> if a == getControllingDirection m 
                                                  then BallOut Goalkick (other a) (getGoalkickPoint (pitch m) (other a) s) True
                                                  else BallOut Cornerkick a (getCorner (pitch m) a s) True 
                      BallOut PreKickoff bn pt True -> if playingPlayers m == numPlayers then BallOut Kickoff bn pt True else oldmatchst
                      BallOut Kickoff bn pt True    -> if kickoffReady m
                                                         then BallOut Kickoff bn pt False 
                                                         else oldmatchst
                      BallOut Kickoff bn pt False   -> if kickoffReady m
                                                         then if ballkicked then BallIn bn else oldmatchst
                                                         else BallOut Kickoff bn pt True
                      BallOut st bn pt True         -> if pitchFreeFromSubstitutes m then BallOut st bn pt False else oldmatchst  -- TODO: refine
                      BallOut _ bn _  False         -> if ballkicked && ballWithinInnerPitch (ball m) (pitch m) then BallIn bn else oldmatchst
    let m' = m{ballplay=nb}
    put m'

playerPositionToString :: PlayerInfo -> String
playerPositionToString p = "(" ++ (show plid) ++ " 0 " ++ (show plx) ++ " " ++ (show ply) ++ " " ++ (show plz) ++ " 0 0 0)"
    where plid            = idnum $ staticplayer p
          (plx, ply, plz) = location $ plentity p

ballPositionToString :: Ball -> String
ballPositionToString b = "(-2 0 " ++ (show blx) ++ " " ++ (show bly) ++ " " ++ (show blz) ++ " 0 0 0)"
    where (blx, bly, blz) = location $ ballentity b

updatePlayerWithControl :: String -> State MatchStatus ()
updatePlayerWithControl = updatePlayerInMatchStatus

updatePlayersWithControls :: [String] -> State MatchStatus ()
updatePlayersWithControls ss = mapM_ updatePlayerWithControl ss

-- |'updatePlayerInMatchStatus' actually applies the controls to the match.
-- That means that if the controls are invalid (too high acceleration,
-- running while injured etc.) they have to be corrected here.
updatePlayerInMatchStatus :: String               -- ^ control
                          -> State MatchStatus () -- ^ new match status
updatePlayerInMatchStatus s = do
    m  <- get
    let p           = getPlayerInfoWithID m (controlPlayeridToken s)
    let pd          = if isNothing p then Up else direction (fromJust p)
    let hbloc       = location $ plentity (fromJust p)
    let newball     = if playerIDAbleToKick m (controlPlayeridToken s) && playerIDAllowedToKick m (controlPlayeridToken s) 
                          then setBallVelocity (controlMoveCoordinatesToken s) (ball m)
                          else ball m
    let heldball    = if playerIDAbleToHoldBall m (controlPlayeridToken s) && playerIDAllowedToHoldBall m (controlPlayeridToken s) && not (isNothing p)
                          then (stopBall . setBallLocation hbloc) (ball m)
                          else ball m
    let newhplayers = Data.Map.adjust (acceleratePlayerToVelocity (controlMoveCoordinatesToken s)) (controlPlayeridToken s) (homeplayers m)
    let newaplayers = Data.Map.adjust (acceleratePlayerToVelocity (controlMoveCoordinatesToken s)) (controlPlayeridToken s) (awayplayers m)
    let newbp       = if playerIDAbleToKick m (controlPlayeridToken s) && playerIDAllowedToKick m (controlPlayeridToken s)
                          then changeBallOwner pd (ballplay m)
                          else ballplay m
    let m' = case inputType s of
               Controls Move    -> m{homeplayers = newhplayers, awayplayers = newaplayers}
               Controls Kick    -> trace ("Ball kicked; velocity=" ++ (show (velocity (ballentity newball)))) (m{ball = newball, ballplay=newbp})
               Controls Tackle  -> m               -- TODO: tackle
               Controls Hold    -> m{homeplayers = newhplayers, awayplayers = newaplayers, ball = heldball, ballplay=newbp}
               _                -> m
    put m'

-- |This function handles new client connections and adds them to match status.
clientHandler :: MatchInfo -> TVar MatchStatus -> Handle -> IO ()
clientHandler c mb h = do
    putStrLn "Starting client handler"
    hPutStrLn h (show c)
    hFlush h
    putStrLn "Sent match info to client"
    ok <- getClientType h
    case ok of
      Human il  -> addNewClients mb h True  il
      AI il     -> addNewClients mb h False il
      _         -> putStrLn "False input from client" >> hClose h

addNewClients :: TVar MatchStatus -> Handle -> Bool -> [Integer] -> IO ()
addNewClients mb h human il = do
    putStrLn $ "Adding new clients: " ++ (show il)
    m <- atomically $ readTVar mb
    let mn = addClientsToMatchStatus m human il h
    if isNothing mn
        then do
          putStrLn "False input parameters from client"
          hClose h
        else do
          atomically $ writeTVar mb (fromJust mn)
          putStrLn ("New client connected: " ++ (show il))
          doGame h

-- | 'dataHandler' handles incoming data from clients and uses
-- it to update the match status (i.e. player actions).
dataHandler :: TChan String           -- ^ control channel
            -> TVar MatchStatus       -- ^ match status
            -> Handle                 -- ^ client handle
            -> String                 -- ^ data from the client
            -> IO ()                  -- ^ return
dataHandler c mb h s | seq c $ seq mb $ seq h $ seq s $ False = undefined
dataHandler c mb h s | length s == 0 = return ()
                     | otherwise     = do
    m <- atomically $ readTVar mb
--    putStrLn ("Received: " ++ s)
    let ss = mapMaybe (controlEventHandler m h) (events s)
    mapM (\x -> atomically (writeTChan c x)) ss       -- <- mem leak <-
--    mapM_ (controlEventHandler c m h) (events s)
    return ()

-- | 'controlEventHandler' handles control events (surprise)
-- by verifying them and returning the events.
controlEventHandler :: MatchStatus        -- ^ match status
                    -> Handle             -- ^ client handle
                    -> String             -- ^ data from the client
                    -> Maybe String       -- ^ just valid data, or nothing
controlEventHandler m h s | seq m $ seq h $ seq s $ False = undefined
controlEventHandler m h s = if valid then Just s else Nothing
    where valid = not (null s) && not (isNothing plh) && h == (fromJust plh)
          plh   = getPlayerHandle m (controlPlayeridToken s)

getClientType :: Handle -> IO ClientType
getClientType h = do
    putStrLn "Waiting for OK"
    s <- hGetLine h
    putStrLn ("Received: " ++ s)
    idl <- evaluate (read (drop 2 s)) `catch` \_ -> return []
    case take 2 s of
      "OK"  -> return (Human idl)
      "AI"  -> return (AI    idl)
      _     -> return InvalidClient

-- | 'doGame' sends the match initialization data to the client.
doGame :: Handle -> IO ()
doGame h = do
    hPutStrLn h "B 0 0"
    hPutStrLn h "C 0 0"

getLocation :: MatchStatus -> Integer -> Vector3
getLocation m i | seq m $ seq i $ False = undefined
getLocation m i = if not (isNothing homep) 
                      then location $ plentity $ fromJust homep
                      else location $ plentity $ fromMaybe (error "Function getLocation: no key found.") awayp
    where homep = Data.Map.lookup i (homeplayers m)
          awayp = Data.Map.lookup i (awayplayers m)

readStrict :: FilePath -> IO String
readStrict f = do
    c <- withFile f ReadMode hGetContentsStrict
    putStrLn ("read characters: " ++ (show (length c)))
    putStrLn ("file name: " ++ f)
    return c

hGetContentsStrict :: Handle -> IO String
hGetContentsStrict h = do
    fin <- hIsEOF h
    if fin then return ("") else do
                            thisline <- hGetLine h
                            rest <- hGetContentsStrict h
                            return (thisline ++ rest)
