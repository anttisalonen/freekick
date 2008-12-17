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

#include "DynamicEntity.h"

#include "Player.h"
#include "MatchPlayer.h"
#include "MatchBall.h"
#include "MatchClub.h"
#include "MatchData.h"
#include "MatchIDs.h"

#include "messages/ConstantUpdateMessage.h"

namespace freekick
{
    namespace match
    {
        using namespace soccer;
        class MatchStatus
        {
        public:
            MatchStatus(boost::shared_ptr<MatchData> md);
            virtual ~MatchStatus() { }
/*
            void newEvent (const std::string& evt );
            void newEvents (std::vector <std::string>& events );
*/
            void update(const messages::ConstantUpdateMessage& m);
            void update(const std::vector<messages::ConstantUpdateMessage>& ms);
            boost::shared_ptr<MatchData> getMatchData() const;

            void getEntities (std::vector <boost::shared_ptr<DynamicEntity> >& v);

            // void addPlayer(const std::string& clubname, int idnum, const Color& col);
            // void addBall();
            // void updateAll(float interval);
            // void interpolateAll(boost::posix_time::ptime pt);

        private:
            boost::shared_ptr<MatchData> mMatchData;
            std::map <int, boost::shared_ptr<DynamicEntity> > mEntities;
            std::vector <boost::shared_ptr<DynamicEntity> > mEntityVector;
            // TODO: add status of yellow cards etc.

            Time currtime;
            unsigned int score_home;
            unsigned int score_away;
            unsigned int injurytime;
            bool secondhalf;
        };
    }
}

#endif
