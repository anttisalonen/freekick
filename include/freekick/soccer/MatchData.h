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


#ifndef MATCHDATA_H
#define MATCHDATA_H

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
#include <boost/serialization/shared_ptr.hpp>

#include "Time.h"
#include "Stadium.h"
#include "Ball.h"
#include "Referee.h"
#include "Club.h"
#include "Lineup.h"

namespace freekick
{
    namespace soccer
    {
        using namespace addutil;
        class MatchData
        {
        public:
            MatchData (boost::shared_ptr<Club> cl1, 
                       boost::shared_ptr<Club> cl2);
            virtual ~MatchData();

            boost::shared_ptr<Club> getHomeClub() const;
            boost::shared_ptr<Club> getAwayClub() const;
            void getHomeClubName(std::string& s) const;
            void getAwayClubName(std::string& s) const;
            template <typename ContT> void getHomePlayerIDs(ContT& ids) const;
            template <typename ContT> void getAwayPlayerIDs(ContT& ids) const;
            boost::shared_ptr<Ball> getBall() const;
            // void getHomeLineup(boost::shared_ptr<Lineup>& s) const;
            // void getAwayLineup(boost::shared_ptr<Lineup>& s) const;
            // void setHomeLineup(const boost::shared_ptr<Lineup> l);
            // void setAwayLineup(const boost::shared_ptr<Lineup> l);

        private:
            Stadium* stadium;
            Referee* referee;
            boost::shared_ptr<Club> homeclub;
            boost::shared_ptr<Club> awayclub;
            boost::shared_ptr<Ball> ball;
            // boost::shared_ptr<Lineup> homelineup;
            // boost::shared_ptr<Lineup> awaylineup;
            Time starttime;

/*
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
*/
        };
    }
}

#endif // MATCHDATA_H
