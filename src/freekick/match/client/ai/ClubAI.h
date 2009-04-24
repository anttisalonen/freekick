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


#ifndef AI_CLUBAI_H
#define AI_CLUBAI_H

#include <vector>

#include <boost/shared_ptr.hpp>
#include <boost/tuple/tuple.hpp>
#include <boost/foreach.hpp>

#include "addutil/Vector3.h"

#include "Primitives.h"
#include "MatchStatus.h"

#include "AIConfig.h"

namespace freekick
{
    namespace match
    {
        namespace client
        {
            namespace ai_client
            {
                typedef boost::tuple<float, float> PlayerPoint;
                typedef std::map<int, PlayerPoint> PlayerPointList;
                typedef boost::shared_ptr<PlayerPointList> PlayerPointListPtr;
                class ClubAI
                {
                public:
                    ClubAI(soccer::BallOwner we, boost::shared_ptr<MatchStatus> ms);
                    virtual ~ClubAI() { }
                    PlayerPointListPtr getPlayerPoints() const;
                    void updateAll();
                    soccer::BallOwner ourClub() const { return mOurClub; }
                    bool isBlocked() const { return mBlocked; }
                    bool isKickoff() const { return mIskickoff; }
                    boost::tuple<int, float> nearestOwn() const { return mNearestown; }
                    boost::tuple<int, float> nearestOther() const { return mNearestother; }
                    bool ballOnOurSide() const { return mBallonourside; }
                    bool ourClubHasBall() const { return mOurclubhasball; }
                    bool ballInOurGoalArea() const { return mBallinourgoalarea; }
                    bool ballIsHeld() const { return mBallisheld; }
                    bool weAreOwner() const { return mWeAreOwner; }
                    BallInOut getBio() const { return mBio; }
                    addutil::Vector3 ballPosCorrected() const { return mBallpos_corrected; }
                    addutil::Vector3 ballFuturePos() const { return mBallFuturePos; }
                protected:
                    void updatePlayerPoints();
                    void updatePlayerPoint(int plid, const addutil::Vector3& plpos);
                    float playerPosToScore(const addutil::Vector3& plpos) const;
                private:
                    soccer::BallOwner mOurClub;
                    boost::shared_ptr<MatchStatus> mMatchStatus;
                    PlayerPointListPtr mPlayerPoints;
                    std::vector<boost::shared_ptr<MatchPlayer> > mOwnPlayers;
                    std::vector<boost::shared_ptr<MatchPlayer> > mOppPlayers;

                    BallState mBss;
                    BallInOut mBio;
                    bool mIskickoff;
                    bool mBlocked;
                    addutil::Vector3 mBallpos_corrected;
                    boost::tuple<int, float> mNearestown;
                    boost::tuple<int, float> mNearestother;
                    bool mBallonourside;
                    bool mOurclubhasball;
                    bool mBallinourgoalarea;
                    bool mBallisheld;
                    bool mOwnerClub;
                    bool mWeAreOwner;
                    addutil::Vector3 mBallFuturePos;
                };
            }
        }
    }
}

#endif
