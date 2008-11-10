module Common
where

import Libaddutil.Vector
import Libaddutil.Entity

import Freekick.Libsoccer.Pitch
import Freekick.Libsoccer.Formation
import Freekick.Libsoccer.Lineup

import Freekick.Libmatch.MatchStatus
import Freekick.Libmatch.PlayerInfo

data AIAction = PassAIAction PlayerInfo Vector3 Ball
              | ScoreAIAction Pitch Ball
              | DribbleAIAction Vector3 Ball
              | FetchAIAction Entity
              | GotoFormationAIAction Pitch Formation Lineup
              | StayAIAction
              | GotoPointAIAction Vector3
              | HoldBallAIAction Vector3
