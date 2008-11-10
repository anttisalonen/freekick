module Freekick.Libmatch.Control
where

import Freekick.Libmatch.MatchStatus
import Freekick.Libmatch.Rules
import Freekick.Libmatch.PlayerInfo

import Libaddutil.Vector
import Libaddutil.Entity

import System.IO
import Control.Concurrent.STM
import Control.Monad.State
import qualified Data.Map
import Data.Maybe
import Char

data InputType = Invalid
               | Update
               | Status StatusType
               | Controls ControlType
     deriving (Show, Eq, Read)

data ControlType = Move
                 | Kick
                 | Tackle
                 | Hold
     deriving (Show, Eq, Read)

data UpdateType = UpdateLocation
                | UpdateVelocity
                | UpdateAcceleration

inputType :: String -> InputType
inputType s | length s < 1 = Invalid
            | otherwise    = if isDigit cc || isDigit (head (tail s))
                                 then Update
                                 else case cc of
                                        'a' -> Controls Move
                                        'b' -> Controls Kick
                                        'c' -> Controls Tackle
                                        'd' -> Controls Hold
                                        'A' -> Status Pause
                                        'B' -> Status Time
                                        'C' -> Status Score
                                        'D' -> Status Match
                                        _   -> Invalid
            where cc = head s

controlPlayeridToken :: String -> Integer
controlPlayeridToken s = 
    case inputType s of
      Controls _ -> read (last (words s)) 
      _          -> 0

controlMoveCoordinatesToken :: String -> Vector3
controlMoveCoordinatesToken s =
    case inputType s of
      Controls _ -> (x, y, z)
      _          -> (0, 0, 0)
    where x = read $ (words s) !! 1
          y = read $ (words s) !! 2
          z = read $ (words s) !! 3

events :: String -> [String]
events ""  = []
events ")" = []
events s   = e : (events rest)
    where (e, rest) = span (/= ')') (drop 1 (dropWhile (/= '(') s))

getUpdateType :: String -> UpdateType
getUpdateType s = case ut of
                    0        -> UpdateLocation
                    1        -> UpdateVelocity
                    _        -> UpdateAcceleration
    where ut = ((read . head . tail . words) s) :: Int

getUpdateCoordinates :: String -> Vector3
getUpdateCoordinates s = (x, y, z)
    where x = read $ (words s) !! 2
          y = read $ (words s) !! 3
          z = read $ (words s) !! 4

applyEventOnPlayer :: String -> PlayerInfo -> PlayerInfo
applyEventOnPlayer s p = p{plentity=applyEventOnEntity s (plentity p)}

applyEventOnBall :: String -> Ball -> Ball
applyEventOnBall s b = b{ballentity = applyEventOnEntity s (ballentity b)}

applyEventOnEntity :: String -> Entity -> Entity
applyEventOnEntity s e = case (getUpdateType s) of
                           UpdateLocation -> setEntityLocation      e (getUpdateCoordinates s)
                           UpdateVelocity -> setEntityVelocity      e (getUpdateCoordinates s)
                           _              -> setEntityAcceleration  e (getUpdateCoordinates s)

playerAbleToKick :: PlayerInfo -> Ball -> Bool
playerAbleToKick p b = vectorLength (vectorFromTo (plentity p) (ballentity b)) <= maxKickDistance

playerAbleToHoldBall :: PlayerInfo -> Ball -> Bool
playerAbleToHoldBall = playerAbleToKick

playerIDAbleToKick :: MatchStatus -> Integer -> Bool
playerIDAbleToKick m i = if isNothing pfound then False else playerAbleToKick (fromJust pfound) (ball m)
    where pfound = getPlayerInfoWithID m i

playerIDAbleToHoldBall :: MatchStatus -> Integer -> Bool
playerIDAbleToHoldBall m i = if isNothing pfound then False else playerAbleToHoldBall (fromJust pfound) (ball m)
    where pfound = getPlayerInfoWithID m i

updateBallPlay :: String -> BallPlay
updateBallPlay e | length (words e) < 6 = BallOut PreKickoff Up (0, 0, 0) True
                 | otherwise            = if fnum == 0 then BallIn bdir else BallOut btype bdir (bpx,0,bpy) blocked
    where btype = case fnum of
                    1        -> PreKickoff
                    2        -> Kickoff
                    3        -> Throwin
                    4        -> Goalkick
                    5        -> Cornerkick
                    6        -> IndirectFreekick
                    7        -> DirectFreekick
                    8        -> Penaltykick
                    9        -> DroppedBall
                    10       -> HalfFullTime
                    _        -> PreKickoff
          blocked = if snum == 0 then False else True
          fnum = (read $ words e !! 1) :: Int
          snum = (read $ words e !! 5) :: Int
          bdir = if bdn == 0 then Down else Up
          bdn  = (read $ words e !! 2) :: Int
          bpx  = read $ words e !! 3
          bpy  = read $ words e !! 4

ballPlayToEvent :: BallPlay -> String
ballPlayToEvent b = "(D " ++ bs ++ " " ++ ow ++ " " ++ px ++ " " ++ py ++ " " ++ rs ++ ") "
    where (bs, ow, px, py, rs) = case b of
                 BallIn  pd                                   -> ("0", spd, "0", "0", "0")
                     where          spd = if pd == Down  then "0" else "1"
                 BallOut bt pd (bx, _, bz) bl -> (sbt, spd, show (fromEnum bx), show (fromEnum bz), sbl)
                     where          sbt = case bt of
                                            PreKickoff          -> "1"
                                            Kickoff             -> "2"
                                            Throwin             -> "3"
                                            Goalkick            -> "4"
                                            Cornerkick          -> "5"
                                            IndirectFreekick    -> "6"
                                            DirectFreekick      -> "7"
                                            Penaltykick         -> "8"
                                            DroppedBall         -> "9"
                                            HalfFullTime        -> "10"
                                    spd = if pd == Down  then "0" else "1"
                                    sbl = if bl == False then "0" else "1"

kickToEvent :: Vector3 -> Integer -> String
kickToEvent v plid = coordinateEvent 'b' v plid

tackleToEvent :: Integer -> String
tackleToEvent plid = "(" ++ "c" ++ " " ++ (show plid) ++ ") "

runToEvent :: Vector3 -> Integer -> String
runToEvent v plid =  coordinateEvent 'a' v plid

holdBallToEvent :: Vector3 -> Integer -> String
holdBallToEvent v plid = coordinateEvent 'd' v plid

coordinateEvent :: Char -> Vector3 -> Integer -> String
coordinateEvent c (x,y,z) plid = "(" ++ [c] ++ " " ++ (show x) ++ " " ++ (show y) ++ " " ++ (show z) ++ " " ++ (show plid) ++ ") "

-- | 'eventHandler' updates match status after receiving data from the physics
-- server.
eventHandler :: TVar MatchStatus -> String -> Handle -> IO ()
eventHandler mb s _ = do
--    putStrLn "Event handler!"
--    putStrLn ((unlines (events s)) ++ "finish")
    m <- atomically $ readTVar mb
--    when (length (events s) > 2) $ putStrLn $ show $ last $ init $ events s
    let mnew = handleEvents (events s) m
    when (ballplay mnew /= ballplay m) (putStrLn (show (ballplay mnew)))
    atomically $ writeTVar mb mnew

handleEvents :: [String] -> MatchStatus -> MatchStatus
handleEvents []     m = m
handleEvents (e:es) m = handleEvents es (handleEvent m e)

handleEvent :: MatchStatus -> String -> MatchStatus
handleEvent m s | seq m $ seq s $ False = undefined
handleEvent m s | length s < 1 = m
                | otherwise    = 
                case inputType s of
                  Invalid      -> m
                  Update       -> m{homeplayers = hpl, awayplayers = apl, ball=nb}
                  Status Pause -> m
                  Status Time  -> m
                  Status Score -> m
                  Status Match -> m{ballplay = nbp}
                  _            -> m
    where upplid    = if inputType s == Update then read $ head $ words s else 0
          hpl       = Data.Map.adjust (applyEventOnPlayer s) upplid (homeplayers m)
          apl       = Data.Map.adjust (applyEventOnPlayer s) upplid (awayplayers m)
          nb        = if upplid == (-2) then applyEventOnBall s (ball m) else ball m
          nbp       = updateBallPlay s

