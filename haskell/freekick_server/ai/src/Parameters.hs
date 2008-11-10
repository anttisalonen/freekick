module Parameters
where

maxPassDistance :: Float
maxPassDistance = 40

maxPassVelocity :: Float
maxPassVelocity = 10      -- ^ [m/s]

-- | How many closest opponents are taken into account.
maxClosestThreats :: Int
maxClosestThreats = 2

-- | Opponent farther away (in meters) -> not a threat. Used for norming.
maxThreatDistance :: Float
maxThreatDistance = 10

-- | This value is used to calculate just how dangerous an opponent is based
-- on distance; threat = normed_distance^assumed_aggressiveness.
-- See graphs of roots f(x)=x^[1/2,1,2,etc.] <- x between 0 and 1.
threatAssumedAggressiveness :: Float
threatAssumedAggressiveness = 1

-- | Exponential, see threatAssumedAggressiveness.
spotValueFactor :: Float
spotValueFactor = 0.5

-- | The distance how far to run when considering the best support spot.
maxRunDistance :: Float
maxRunDistance = 20

-- | The distance how far to dribble at once.
dribbleDistance :: Float
dribbleDistance = 5

-- | Multiplier for scoring possibility. Higher makes the play more aggressive.
scoreCoefficient :: Float
scoreCoefficient = 0.6

-- | Multiplier for dribbling possibility.
dribbleCoefficient :: Float
dribbleCoefficient = 0.6

passStrength :: Float    -- ^distance to where to pass
             -> Float    -- ^vector length
passStrength d = 40 * d

scoreStrength :: Float   -- ^distance to goal
              -> Float   -- ^vector length
scoreStrength d = 120 * d + 1000

runStrength :: Float
runStrength = 1
