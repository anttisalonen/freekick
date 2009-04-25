/************************************************************************
  This file is part of Freekick.

  Freekick is free software: you can redistribute it and/or modify
  it under the terms of the GNU Affero General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  Freekick is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU Affero General Public License for more details.

  You should have received a copy of the GNU Affero General Public License
  along with Freekick.  If not, see <http://www.gnu.org/licenses/>.

  Copyright Antti Salonen, 2008
**************************************************************************/

#include "ClubAI.h"

namespace freekick
{
    namespace match
    {
        namespace client
        {
            namespace ai_client
            {
                ClubAI::ClubAI(soccer::BallOwner we, boost::shared_ptr<MatchStatus> ms)
                    : mOurClub(we),
                      mMatchStatus(ms),
                      mPlayerPoints(new PlayerPointList())
                {
                }

                PlayerPointListPtr ClubAI::getPlayerPoints() const
                {
                    return mPlayerPoints;
                }

                void ClubAI::updateAll()
                {
                    mBss = mMatchStatus->getBallState();
                    mBio = mBss.bio_type;
                    mIskickoff = mBio == Kickoff;
                    mBlocked = mBss.blocked_play;
                    mBallpos_corrected = mMatchStatus->absolute_pitch_position_to_percent(mMatchStatus->getBall()->getPosition(), mOurClub);
                    mNearestown = mMatchStatus->nearestPlayerFromClubToBall(mOurClub);
                    mNearestother = mMatchStatus->nearestPlayerFromClubToBall(other(mOurClub));
                    mBallonourside = mMatchStatus->onOwnSide(mMatchStatus->getBall()->getPosition(), mOurClub);
                    mOurclubhasball = mNearestown.get<1>() <= mNearestother.get<1>();
                    mBallinourgoalarea = ownersGoal(mOurClub, mMatchStatus->ballInGoalArea());
                    mBallisheld = mMatchStatus->holdingBall() != 0;
                    mOwnerClub = mBss.owner;
                    mWeAreOwner = mBss.owner == mOurClub;
                    mBallFuturePos = mMatchStatus->getBall()->getFuturePosition(AIConfig::getInstance()->future_lookup_time);
                    mTarget = ballOwnerToPlayerTarget(mOurClub, mMatchStatus->isSecondHalf());
                    mOwnGoal = mMatchStatus->getGoalPosition(other(mTarget));
                    mTargetGoal = mMatchStatus->getGoalPosition(mTarget);
                    mBallPos = mMatchStatus->getBall()->getPosition();
                    mBallDesire = std::max(
                            addutil::general::normalize(desire_max_dist, (mBallPos - mOwnGoal).length()),
                            addutil::general::normalize(desire_max_dist, (mBallPos - mTargetGoal).length()));

                    updatePlayerPoints();
                }

                void ClubAI::updatePlayerPoints()
                {
                    mPlayerPoints->clear();
                    mMatchStatus->getPlayers(mOwnPlayers, mOurClub);
                    mMatchStatus->getPlayers(mOppPlayers, soccer::other(mOurClub));
                    BOOST_FOREACH(boost::shared_ptr<MatchPlayer> p, mOwnPlayers)
                    {
                        updatePlayerPoint(p->getID(), p->getPosition());
                    }
                }

                void ClubAI::updatePlayerPoint(int plid, const addutil::Vector3& plpos)
                {
                    float gen_val = playerPosToScore(plpos);
                    (*mPlayerPoints)[plid] = boost::tuple<int, int>(gen_val, 0.0f);
                    // TODO: second in tuple should be the risk
                    // calculate that by checking how far the opponents are
                }

                float ClubAI::playerPosToScore(const addutil::Vector3& plpos) const
                {
                    float zz = mMatchStatus->absolute_pitch_position_to_percent(plpos, mOurClub).z;
                    // TODO: add more positive values for nearer to centre
                    return zz;
                }
            }
        }
    }
}

