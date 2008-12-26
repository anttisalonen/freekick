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

#include "MatchData.h"

namespace freekick
{
    namespace soccer
    {
        MatchData::MatchData (boost::shared_ptr<Club> cl1, 
                              boost::shared_ptr<Club> cl2,
                              boost::shared_ptr<Stadium> s)
            : homeclub(cl1),
              awayclub(cl2),
              stadium(s),
              ball(new Ball(0.4))     // don't forget the ball
        {
        }

        MatchData::~MatchData()
        {
        }

        boost::shared_ptr<Club> MatchData::getHomeClub() const
        {
            return homeclub;
        }

        boost::shared_ptr<Club> MatchData::getAwayClub() const
        {
            return awayclub;
        }

        void MatchData::getHomeClubName(std::string& s) const
        {
            s = homeclub->getName();
        }

        void MatchData::getAwayClubName(std::string& s) const
        {
            s = awayclub->getName();
        }

        template <typename ContT>
        void MatchData::getHomePlayerIDs(ContT& ids) const
        {
            homeclub->getPlayerIDs(ids);
        }

        template <typename ContT>
        void MatchData::getAwayPlayerIDs(ContT& ids) const
        {
            awayclub->getPlayerIDs(ids);
        }

        boost::shared_ptr<Ball> MatchData::getBall() const
        {
            return ball;
        }

        void MatchData::setStadium(boost::shared_ptr<Stadium>& stad)
        {
            stadium = stad;
        }

        boost::shared_ptr<Stadium> MatchData::getStadium() const
        {
            return stadium;
        }
    }
}
