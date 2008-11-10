module Freekick.Libsoccer.Stage
where

import Freekick.Libsoccer.Trophy

data Stage
   = League   { name                 :: String,
                stagesetup           :: StageSetup,
                leaguesetup          :: LeagueSetup,
                promotions           :: [(Int, StageTarget)],
                relegations          :: [(Int, StageTarget)],
                attendance           :: [(Int, Int, StageTarget)],
                trophy               :: Trophy
              }
   | Knockout { name                 :: String,
                stagesetup           :: StageSetup,
                knockoutsetup        :: KnockoutSetup,
                promotiontarget      :: StageTarget,
                relegationtarget     :: StageTarget,
                trophy               :: Trophy
              }
     deriving (Show, Eq)

data StageSetup
   = StageSetup { seeded          :: Bool,
                  participantnum  :: Int, 
                  legs            :: Int
                }
     deriving (Show, Eq)

data LeagueSetup
   = LeagueSetup { pointsperwin   :: Int,
                   groups         :: Int
                 }
     deriving (Show, Eq)

data KnockoutSetup
   = KnockoutSetup { extratime     :: Bool,
                     penalties     :: Bool,
                     awaygoalsrule :: Bool,
                     replays       :: Bool
                   }
     deriving (Show, Eq)

type StageTarget = (String, String)

getNumberOfPromotions :: Stage -> Int
getNumberOfPromotions  l  = length $ promotions l

getNumberOfRelegations :: Stage -> Int
getNumberOfRelegations l  = length $ relegations l

createLeague :: String -> Stage
createLeague nm = League nm nullStageSetup nullLeagueSetup [] [] [] nullTrophy

nullStageSetup :: StageSetup
nullStageSetup = StageSetup False 0 0

nullLeagueSetup :: LeagueSetup
nullLeagueSetup = LeagueSetup 0 0

nullKnockoutSetup :: KnockoutSetup
nullKnockoutSetup = KnockoutSetup False False False False

stdKnockoutSetup :: KnockoutSetup
stdKnockoutSetup  = KnockoutSetup True  True  False False

countMatchesPerClubInStage :: Stage -> Int
countMatchesPerClubInStage League{stagesetup=ss,leaguesetup=ls}     = (numLeagueGamesPerRound ss ls) * legs ss
countMatchesPerClubInStage Knockout{stagesetup=ss,knockoutsetup=ks} = legs ss + if replays ks == True then 1 else 0

numLeagueGamesPerRound :: StageSetup -> LeagueSetup -> Int
numLeagueGamesPerRound StageSetup{participantnum=n} LeagueSetup{groups=g} | g <= 0    = 0
                                                                          | otherwise = ceiling (toEnum n / toEnum g) - 1

totalMatchesPerClubInStages :: [Stage] -> Int
totalMatchesPerClubInStages s = sum $ map countMatchesPerClubInStage s

createStdStagesFromClubNumber :: String -> Int -> [Stage]
createStdStagesFromClubNumber tn n = createStdStagesFromClubNumber' tn n 1

createStdStagesFromClubNumber' :: String -> Int -> Int -> [Stage]
createStdStagesFromClubNumber' tn n m | n <  2    = []
                                      | n == 2    = finstage "Final" "" n : []
                                      | n == 4    = (stdstage "Semi-finals" "Final" n) : createStdStagesFromClubNumber' tn (n `div` 2) (m + 1)
                                      | n == 8    = (stdstage "Quarterfinals" "Semi-finals" n) : createStdStagesFromClubNumber' tn (n `div` 2) (m + 1)
                                      | n == 16   = (stdstage ((show m) ++ ". round") "Quarterfinals" n) : createStdStagesFromClubNumber' tn (n `div` 2) (m + 1)
                                      | otherwise = (stdstage ((show m) ++ ". round") ((show (m + 1)) ++ ". round") n) : createStdStagesFromClubNumber' tn (n `div` 2) (m + 1)
    where stdstage n1 n2 n3 = Knockout n1 (StageSetup False n3 2) stdKnockoutSetup (tn, n2) ("", "") nullTrophy
          finstage n1 n2 n3 = Knockout n1 (StageSetup False n3 1) stdKnockoutSetup (tn, n2) ("", "") nullTrophy

printStageInfo :: Stage -> String
printStageInfo League{Freekick.Libsoccer.Stage.name=ln, stagesetup=ss, leaguesetup=ls} = ln ++ "\n" ++ printStageSetupInfo (ss) ++ printLeagueSetupInfo (ls)
printStageInfo Knockout{Freekick.Libsoccer.Stage.name=kn, stagesetup=ss, promotiontarget=pt, relegationtarget=rt} = kn ++ "\n" ++ printStageSetupInfo (ss) ++ printStageTarget "Promotions:" pt ++ printStageTarget "Relegations:" rt

printStageSetupInfo :: StageSetup -> String
printStageSetupInfo s = "Seeded: " ++ (show (seeded s)) ++ "\nParticipants: " ++ (show (participantnum s)) ++ "\nLegs: " ++ (show (legs s)) ++ "\n"

printLeagueSetupInfo :: LeagueSetup -> String
printLeagueSetupInfo l = ""     -- TODO

printStageTarget :: String -> StageTarget -> String
printStageTarget n (t, s) = n ++ tt ++ st ++ "\n"
    where tt = if t == "" then "" else "\nTournament: " ++ t 
          st = if s == "" then "" else "\nStage: " ++ s
