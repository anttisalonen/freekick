module Main
where
 
import System.IO
import Control.Monad.Reader
import Prelude hiding (catch)
import Control.Concurrent
import Control.Concurrent.STM
import Data.List

import Libaddutil.Net
import Libaddutil.ListUtils

import Freekick.Libsoccer.MatchInfo
import Freekick.Libsoccer.Club

import Freekick.Libmatch.MatchStatus
import Freekick.Libmatch.PlayerInfo
import Freekick.Libmatch.Control
import Freekick.Libmatch.Rules

import Actions
import Support
import Common

server :: String
server = "127.0.0.1"

port :: Int
port   = 32105

main :: IO ()
main = connect server port start
 
start :: Handle -> IO ()
start h = do
    t <- hGetLine h
    let c :: MatchInfo
        c = read t
    let m = createMatchStatusFromMatchInfo c
    matchBox <- atomically $ newTVar m
    putStrLn $ name $ homecl c
    putStrLn $ name $ awaycl c
    let pllist = getEmptyPlayerIDs m
    putStrLn ("Players: " ++ (show pllist))
    send ("\nAI " ++ (show pllist)) h
    forkIO $ controlsSender h matchBox pllist 100000
    listen h (eventHandler matchBox)

-- | sends all controls from the controlled players to the physics
-- server on regular intervals. the controls are also
-- created here.
controlsSender :: Handle              -- ^ physics handle
               -> TVar MatchStatus    -- ^ match status
               -> [Integer]           -- ^ list of player ids
               -> Int                 -- ^ call interval in microseconds
               -> IO ()               -- ^ return
controlsSender h mb ids i = do
--    putStrLn "Controls sender!"
    m <- atomically $ readTVar mb
    let dc = createControls m ids
--    putStrLn (show $ ballentity (ball m))
--    putStrLn dc
    hPutStrLn h dc
    when (i > 0) $ do
        threadDelay i
        controlsSender h mb ids i

createControls :: MatchStatus -> [Integer] -> String
createControls m ids = concat $ map (createControl m) (lookupMany (homeplayers m) ids ++ lookupMany (awayplayers m) ids)

-- | 'createControl' is the brain of the AI.
createControl :: MatchStatus -> PlayerInfo -> String
createControl m p | playerstatus p == Playing  = case ballplay m of
                      BallIn _                     -> duringMatchAction m p
                      BallOut PreKickoff d _ _     -> runToKickoffFormationAction p (pitch m) myf myl d
                      BallOut Kickoff    d _ True  -> runToKickoffFormationAction p (pitch m) myf myl d
                      BallOut Kickoff    d _ False -> handleSpecialSituationAction m p d ""
                      BallOut Throwin d _ True     -> throwinSupportAction m p d
                      BallOut Throwin d _ False    -> handleSpecialSituationAction m p d (throwinSupportAction m p d)
                      BallOut _ d _ False          -> handleSpecialSituationAction m p d ""    -- TODO: expand
                      _                            -> ""
                  | playerstatus p == Observer = runToKickoffFormationAction p (pitch m) myf myl Up
                  | otherwise                  = runToKickoffFormationAction p (pitch m) myf myl Up
    where myf   = playerFormation m p
          myl   = playerLineup m p

handleSpecialSituationAction :: MatchStatus -> PlayerInfo -> PitchDirection -> String -> String
handleSpecialSituationAction m p d s = if direction p == d && isNearestToBall p teammates (ball m)
                                         then actionToString p (FetchAIAction (ballentity (ball m))) -- duringMatchAction m p
                                         else s
    where teammates = getPlayingTeammatesFromMatchStatus m p

duringMatchAction :: MatchStatus -> PlayerInfo -> String
duringMatchAction m p = if playerAbleToKick p (ball m) then withBallAction p teammates opponents (pitch m) (ball m) (ballplay m) else withoutBallAction p m
    where teammates = getPlayingTeammatesFromMatchStatus m p
          opponents = getPlayingOpposingTeam m p

