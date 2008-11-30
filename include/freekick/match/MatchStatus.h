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
  This file was generated on So Okt 26 2008 at 12:09:20
**************************************************************************/


#ifndef MATCHSTATUS_H
#define MATCHSTATUS_H

#include <vector>
#include <set>
#include <string>
#include <iostream>
#include <sstream>
#include <exception>

#include <boost/foreach.hpp>
#include <boost/shared_ptr.hpp>
#include <boost/thread.hpp>
#include <boost/serialization/map.hpp>
#include <boost/serialization/set.hpp>

#include "Entity.h"
#include "Time.h"
#include "MatchStadium.h"
#include "MatchBall.h"
#include "MatchReferee.h"
#include "MatchClub.h"

/**
 * class Status
 */

namespace freekick
{
    namespace match
    {
        using namespace addutil;
        using namespace freekick::soccer;
        using namespace freekick::match;
        class MatchStatus
        {
        public:
            
            // Constructors/Destructors
            //  

            /**
             */
            MatchStatus ( );
            virtual ~MatchStatus();

            // Public member functions

            /**
             * @param  evt
             */
            void newEvent (const std::string& evt );

            /**
             * @param  events
             */
            void newEvents (std::vector <std::string>& events );

            std::set <boost::shared_ptr<addutil::Entity> >* getEntities ( );

            void addClub(const std::string& name);
            void addPlayer(const std::string& clubname, int idnum, const Color& col);
            void addBall();
            void updateAll(float interval);
            void interpolateAll(boost::posix_time::ptime pt);

            bool run();
//     const std::string& getHomeClubName() const;
            template <typename ContT> void getHomePlayerIDs(ContT& ids);
            template <typename ContT> void getAwayPlayerIDs(ContT& ids);

        private:

            // Private attributes
            //  

            MatchStadium* stadium;
            boost::shared_ptr<MatchBall> ball;
            std::map <std::string, boost::shared_ptr<MatchClub> > clubs;
            MatchReferee* referee;
            std::set <boost::shared_ptr<Entity> > entities;
            boost::shared_ptr<MatchClub> homeclub;
            boost::shared_ptr<MatchClub> awayclub;

            Time currtime;
            unsigned int score_home;
            unsigned int score_away;
            unsigned int injurytime;
            bool secondhalf;

            template <typename ContT> void getClubPlayerIDs(const boost::shared_ptr<MatchClub> c, ContT& ids);

            friend class boost::serialization::access;
            template<class Archive>
                void serialize(Archive & ar, const unsigned int version)
            {
                ar & stadium;
                ar & ball;
                ar & clubs;
                ar & referee;
                ar & entities;
                ar & homeclub;
                ar & awayclub;
                ar & currtime;
                ar & score_home;
                ar & score_away;
                ar & secondhalf;
            }
        };
    }
}

#endif // MATCHSTATUS_H
