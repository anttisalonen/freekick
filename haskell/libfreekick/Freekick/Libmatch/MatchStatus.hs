module Freekick.Libmatch.MatchStatus
where

import Freekick.Libmatch.PlayerInfo
import Freekick.Libmatch.Rules

import Freekick.Libsoccer.Player
import Freekick.Libsoccer.MatchInfo
import Freekick.Libsoccer.Pitch
import Freekick.Libsoccer.Lineup
import Freekick.Libsoccer.Formation

import Libaddutil.ListUtils
import Libaddutil.Primitives
import Libaddutil.Entity

import qualified Data.Map
import System.IO
import Data.Maybe
import Data.List

data MatchStatus = MatchStatus { homeplayers   :: Players
                               , awayplayers   :: Players
                               , homelineup    :: Lineup
                               , awaylineup    :: Lineup
                               , homeformation :: Formation
                               , awayformation :: Formation
                               , home          :: Int
                               , away          :: Int
                               , pitch         :: Pitch
                               , time          :: Time               -- ^current time of day, e.g. 18:00:00
                               , matchtime     :: PreciseTime        -- ^current time of match, e.g. 63:04.237 (millisecond precision)
                               , injurytime    :: Int                -- ^projected injury time in seconds
                               , firsthalf     :: Bool
                               , ball          :: Ball
                               , ballplay      :: BallPlay
                               , clients       :: Data.Map.Map Integer Handle
                               }
    deriving (Eq, Show)    -- no read, because no read Handle -> TODO: implement read without handles

data Ball = Ball { ballentity :: Entity
                 , weight     :: Float
                 }
    deriving (Show, Eq, Read)

ballRadius :: Float
ballRadius = 0.11

instance Entital Ball where
    getEntity = ballentity

-- startMatchStatus :: MatchStatus
-- startMatchStatus = MatchStatus Data.Map.empty Data.Map.empty 0 0 testPitch nullTime True newBall (BallOut PreKickoff Up (getCentreSpot testPitch) True) Up Data.Map.empty

createMatchStatusFromMatchInfo :: MatchInfo -> MatchStatus
createMatchStatusFromMatchInfo m = MatchStatus (Data.Map.fromList (zip hids hplayerinfos)) (Data.Map.fromList (zip aids aplayerinfos)) (hlineup m) (alineup m) (hformation m) (aformation m) 0 0 p (0, 0, 0) (0, 0, 0) 0 True newBall (BallOut PreKickoff Up (getCentreSpot p) True) Data.Map.empty
    where hids         = map Freekick.Libsoccer.Player.getID (homepl m)
          aids         = map Freekick.Libsoccer.Player.getID (awaypl m)
          hplayerinfos = map (createPlayerInfo Up (hlineup m)) (homepl m)
          aplayerinfos = map (createPlayerInfo Down (alineup m)) (awaypl m)
          p            = pitchinfo m

addClientsToMatchStatus :: MatchStatus -> Bool -> [Integer] -> Handle -> Maybe MatchStatus
addClientsToMatchStatus m _ [] _ = Just m
addClientsToMatchStatus m r is h = let allempty     = and (map ((flip Data.Map.notMember) (clients m)) is)
                                       addedclients = insertMany is h (clients m)
                                   in if not r && not allempty then Nothing else Just m{clients=addedclients}

removeClientFromMatchStatus :: MatchStatus -> Handle -> MatchStatus
removeClientFromMatchStatus m h = m{clients=Data.Map.filter (/= h) (clients m)}

getPlayerHandle :: MatchStatus -> Integer -> Maybe Handle
getPlayerHandle m i = Data.Map.lookup i (clients m)

newBall :: Ball
newBall = Ball nullEntity 0.43

-- | Returns the number of currently playing players, in total.
playingPlayers :: MatchStatus -> Int
playingPlayers m = length $ getPlayingPlayers ((Data.Map.elems (homeplayers m)) ++ (Data.Map.elems (awayplayers m)))

getPlayerInfoWithID :: MatchStatus -> Integer -> Maybe PlayerInfo
getPlayerInfoWithID m i = let hp = Data.Map.lookup i (homeplayers m)
                              ap = Data.Map.lookup i (awayplayers m)
                          in if not (isNothing hp) then hp else ap

-- | Returns all the team mates of a player, not including the player himself.
getTeammatesFromMatchStatus :: MatchStatus -> PlayerInfo -> [PlayerInfo]
getTeammatesFromMatchStatus m p = filter (\x -> idnum (staticplayer x) /= idnum (staticplayer p)) cl
    where cl = if Data.Map.member (idnum (staticplayer p)) (homeplayers m) then Data.Map.elems (homeplayers m) else Data.Map.elems (awayplayers m)

allPlayers :: MatchStatus -> [PlayerInfo]
allPlayers m = (Data.Map.elems (homeplayers m)) ++ (Data.Map.elems (awayplayers m))

allPlayingPlayers :: MatchStatus -> [PlayerInfo]
allPlayingPlayers m = getPlayingPlayers (hp ++ ap)
    where hp = Data.Map.elems (homeplayers m)
          ap = Data.Map.elems (awayplayers m)

-- | Returns the playing players from a list of playerinfos.
getPlayingPlayers :: [PlayerInfo] -> [PlayerInfo]
getPlayingPlayers ps = filter (\x -> playerstatus x == Playing) ps

getNotPlayingPlayers :: [PlayerInfo] -> [PlayerInfo]
getNotPlayingPlayers ps = filter (\x -> playerstatus x /= Playing) ps

-- | Returns all the playing team mates of a player, not including the player 
-- himself.
getPlayingTeammatesFromMatchStatus :: MatchStatus -> PlayerInfo -> [PlayerInfo]
getPlayingTeammatesFromMatchStatus m p = getPlayingPlayers (getTeammatesFromMatchStatus m p)

-- | All the players of the player's own team, including substitutes etc.
getPlayersTeam :: MatchStatus -> PlayerInfo -> [PlayerInfo]
getPlayersTeam m p = if Data.Map.member (idnum (staticplayer p)) (homeplayers m) then Data.Map.elems (homeplayers m) else Data.Map.elems (awayplayers m)

-- | All the players of the other team, including substitutes etc.
getOpposingTeam :: MatchStatus -> PlayerInfo -> [PlayerInfo]
getOpposingTeam m p = if Data.Map.member (idnum (staticplayer p)) (homeplayers m) then Data.Map.elems (awayplayers m) else Data.Map.elems (homeplayers m)

-- | Only the currently playing players of the other team.
getPlayingOpposingTeam :: MatchStatus -> PlayerInfo -> [PlayerInfo]
getPlayingOpposingTeam m p = getPlayingPlayers (getOpposingTeam m p)

getPlayingControllingTeam :: MatchStatus -> [PlayerInfo]
getPlayingControllingTeam m = let ht  = (firsthalf m && dir == Up) || (not (firsthalf m) && dir == Down)
                                  dir = getControllingDirection m
                              in if ht then getPlayingPlayers (Data.Map.elems (homeplayers m)) else getPlayingPlayers (Data.Map.elems (awayplayers m))

getPlayingNotControllingTeam :: MatchStatus -> [PlayerInfo]
getPlayingNotControllingTeam m = let ht  = (firsthalf m && dir == Up) || (not (firsthalf m) && dir == Down)
                                     dir = getControllingDirection m
                                 in if ht then getPlayingPlayers (getAwayTeam m) else getPlayingPlayers (getHomeTeam m)

inControllingTeam :: MatchStatus -> PlayerInfo -> Bool
inControllingTeam m p = getControllingDirection m == direction p

getHomeTeam :: MatchStatus -> [PlayerInfo]
getHomeTeam m = Data.Map.elems (homeplayers m)

getAwayTeam :: MatchStatus -> [PlayerInfo]
getAwayTeam m = Data.Map.elems (awayplayers m)

-- | Checks if all of the opposing team are not in the centre circle.
freeForKickoff :: MatchStatus -> Bool
freeForKickoff m = let os = getPlayingNotControllingTeam m
                       ps = map plentity os
                   in not $ or $ map (inCentreSpot (pitch m)) ps

allPlayersOnTheirSide :: MatchStatus -> Bool
allPlayersOnTheirSide m = readyForKickoff upls dpls (pitch m)
    where upls = if (firsthalf m) then apls else hpls
          dpls = if (firsthalf m) then hpls else apls
          hpls = map (location . plentity) (filter (\x -> playerstatus x == Playing) (Data.Map.elems (homeplayers m)))
          apls = map (location . plentity) (filter (\x -> playerstatus x == Playing) (Data.Map.elems (awayplayers m)))

-- | Checks if _everything_ is ready for kickoff.
kickoffReady :: MatchStatus -> Bool
kickoffReady m = allPlayersOnTheirSide m && freeForKickoff m && pitchFreeFromSubstitutes m

homeTeam :: MatchStatus -> PlayerInfo -> Bool
homeTeam m p = (direction p == Up && firsthalf m) || (direction p == Down && not (firsthalf m))

pitchFreeFromSubstitutes :: MatchStatus -> Bool
pitchFreeFromSubstitutes m = let os = getNotPlayingPlayers ((getHomeTeam m) ++ (getAwayTeam m))
                                 ps = map plentity os
                             in not $ or $ map (onPitch (pitch m)) ps

getControllingDirection :: MatchStatus -> PitchDirection
getControllingDirection m = case ballplay m of
                              BallIn d        -> d
                              BallOut _ d _ _ -> d

playerIDAllowedToKick :: MatchStatus -> Integer -> Bool
playerIDAllowedToKick m i = let p = getPlayerInfoWithID m i
                                pd = direction (fromJust p)
                            in if isNothing p then False else 
                                   case ballplay m of
                                     BallIn _                          -> True
                                     BallOut _ _ _ True                -> False
                                     BallOut PreKickoff _ _ _          -> False
                                     BallOut HalfFullTime _ _ False    -> False
                                     BallOut DroppedBall _ _ False     -> True
                                     BallOut _ d _ False               -> pd == d

playerIDAllowedToHoldBall :: MatchStatus -> Integer -> Bool
playerIDAllowedToHoldBall m i = let p  = getPlayerInfoWithID m i
                                    jp = fromJust p
                                    pd = direction jp
                                in if isNothing p then False else
                                       case ballplay m of
                                         BallIn d                  -> (getPlayerPositionInt jp) == 0 && d /= pd && inPenaltyBoxDir (plentity jp) (other pd) (pitch m)
                                         BallOut Throwin d _ _     -> d == pd
                                         BallOut _       _ _ _     -> False

playerFormation :: MatchStatus -> PlayerInfo -> Formation
playerFormation m p = if homeTeam m p then homeformation m else awayformation m

playerLineup :: MatchStatus -> PlayerInfo -> Lineup
playerLineup m p = if homeTeam m p then homelineup m else awaylineup m

finalWhistle :: MatchStatus -> Bool
finalWhistle m = let (mm, _, _) = matchtime m
                 in mm >= 45          -- TODO: more accurate implementation

getEmptyPlayerIDs :: MatchStatus -> [Integer]
getEmptyPlayerIDs m = allplids \\ plplids
    where allplids = map (getID . staticplayer) (allPlayers m)
          plplids  = Data.Map.keys (clients m)

ballWithinInnerPitch :: Ball -> Pitch -> Bool
ballWithinInnerPitch b p = withinInnerPitch (ballentity b) p
