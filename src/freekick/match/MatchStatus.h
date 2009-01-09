/************************************************************************
  This file is part of Freekick.

  Freekick is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  Freekick is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with Freekick.  If not, see <http://www.gnu.org/licenses/>.

  Copyright Antti Salonen, 2008
**************************************************************************/


#ifndef FREEKICK_MATCHSTATUS_H
#define FREEKICK_MATCHSTATUS_H

#include <map>
#include <vector>

#include <boost/shared_ptr.hpp>
#include <boost/foreach.hpp>
#include <boost/array.hpp>
#include <boost/tuple/tuple.hpp>

#include "addutil/DynamicEntity.h"

#include "Lineup.h"
#include "Player.h"
#include "MatchPlayer.h"
#include "MatchBall.h"
#include "MatchClub.h"
#include "MatchData.h"
#include "MatchIDs.h"
#include "BallState.h"

#include "messages/ConstantUpdateMessage.h"
#include "messages/GeneralUpdateStatusMessage.h"

namespace freekick
{
    namespace match
    {
        using namespace soccer;
        typedef std::map <int, boost::shared_ptr<MatchPlayer> > MatchPlayerMap;

        class MatchStatus
        {
        public:
            MatchStatus(boost::shared_ptr<MatchData> md);
            virtual ~MatchStatus() { }
            void update(const messages::ConstantUpdateMessage& m);
            void update(const std::vector<messages::ConstantUpdateMessage>& ms);
            void update(const messages::GeneralUpdateStatusMessage& m);
            void update(const std::vector<messages::GeneralUpdateStatusMessage>& ms);
            const boost::shared_ptr<MatchData>& getMatchData() const;
            void getPlayers (std::map <int, boost::shared_ptr<MatchPlayer> >& v) const;
            boost::shared_ptr<MatchPlayer> getPlayer(int id) const;
            boost::shared_ptr<MatchBall> getBall() const;
            const BallState& getBallState() const;
            soccer::BallOwner getPlayerSide(int id) const;
            boost::shared_ptr<Club> getPlayerClub(int id) const;
            addutil::Vector3 getCentreSpot() const;
            boost::tuple<int, float> nearestPlayerToBall() const;
            boost::tuple<int, float> nearestPlayerFromClubToBall(soccer::BallOwner b) const;
            int getPlayerPositions(std::vector<addutil::Vector3>& ret, soccer::BallOwner b) const;
            float getPitchWidth() const;
            float getPitchLength() const;
            addutil::Vector3 getGoal(soccer::BallOwner b) const;

        private:
            const boost::shared_ptr<MatchData> mMatchData;

            boost::shared_ptr<MatchBall> mBall;
            MatchPlayerMap mPlayers;
            // Note: if you add another container (for referee etc.) here, remember to update MatchStatus::update() function

            unsigned int score_home;
            unsigned int score_away;
            Time currtime;
            bool secondhalf;
            unsigned int injurytime;
            BallState mBallState;
            // TODO: add status of yellow cards etc.
        };
    }
}

#endif
