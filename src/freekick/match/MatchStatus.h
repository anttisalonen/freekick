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


#ifndef FREEKICK_MATCHSTATUS_H
#define FREEKICK_MATCHSTATUS_H

#include <map>
#include <vector>

#include <boost/shared_ptr.hpp>
#include <boost/foreach.hpp>
#include <boost/array.hpp>
#include <boost/tuple/tuple.hpp>
#include <boost/serialization/serialization.hpp>

#include "addutil/DynamicEntity.h"

#include "Primitives.h"
#include "Lineup.h"
#include "Player.h"
#include "MatchPlayer.h"
#include "MatchBall.h"
#include "MatchClub.h"
#include "MatchData.h"
#include "MatchIDs.h"
#include "BallState.h"

#include "messages/ConstantUpdateMessage.h"
#include "messages/GeneralUpdateScoreMessage.h"
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
            void update(const messages::ConstantUpdateMessage& m, float time_interval = 0.0f);
            void update(const std::vector<messages::ConstantUpdateMessage>& ms, float time_interval = 0.0f);
            void update(const messages::GeneralUpdateStatusMessage& m);
            void update(const std::vector<messages::GeneralUpdateStatusMessage>& ms);
            void update(const messages::GeneralUpdateScoreMessage& m);
            void update(const std::vector<messages::GeneralUpdateScoreMessage>& ms);
            const boost::shared_ptr<MatchData>& getMatchData() const;
            void getPlayers (std::map <int, boost::shared_ptr<MatchPlayer> >& v) const;
            boost::shared_ptr<MatchPlayer> getPlayer(int id) const;
            boost::shared_ptr<MatchBall> getBall() const;
            const BallState& getBallState() const;
            soccer::PlayerTarget getPlayerTarget(int id) const;
            soccer::BallOwner getPlayerSide(int id) const;
            boost::shared_ptr<Club> getPlayerClub(int id) const;
            addutil::Vector3 getCentreSpot() const;
            boost::tuple<int, float> nearestPlayerToBall() const;
            boost::tuple<int, float> nearestPlayerFromClubToBall(soccer::BallOwner b) const;
            int getPlayerPositions(std::vector<addutil::Vector3>& ret, int pid) const;
            int getPlayerPositions(std::vector<addutil::Vector3>& ret, soccer::PlayerTarget t) const;
            int getPlayerPositions(std::vector<addutil::Vector3>& ret, soccer::BallOwner b) const;
            float getPitchWidth() const;
            float getPitchLength() const;
            addutil::Vector3 getGoalPosition(soccer::PlayerTarget b) const;
            bool playerAllowedToKick(int id) const;
            bool continuing() const;
            void setContinue(bool c);
            void addHomeScore();
            void addAwayScore();
            int getHomeScore() const;
            int getAwayScore() const;
            GoalQuery ballInGoalArea() const;
            int holdingBall() const;
            void setBallHolder(int h);

        protected:
            void updateEntity(addutil::DynamicEntity* e, const messages::ConstantUpdateMessage& m, float time_interval);

        private:
            const boost::shared_ptr<MatchData> mMatchData;
            bool mContinue;

            boost::shared_ptr<MatchBall> mBall;
            MatchPlayerMap mPlayers;
            // Note: if you add another container (for referee etc.) here, remember to update MatchStatus::update() function

            int score_home;
            int score_away;
            Time currtime;
            bool secondhalf;
            int injurytime;
            BallState mBallState;
            // TODO: add status of yellow cards etc.

            friend class boost::serialization::access;
            template<class Archive>
                void serialize(Archive & ar, const unsigned int version)
            {
                ar & mMatchData;
                ar & mContinue;
                ar & mBall;
                ar & mPlayers;
                ar & score_home;
                ar & score_away;
                ar & currtime;
                ar & secondhalf;
                ar & injurytime;
                ar & mBallState;
            }
        };
    }
}

#endif
