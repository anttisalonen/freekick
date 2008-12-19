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
                              boost::shared_ptr<Club> cl2)
            : homeclub(cl1),
              awayclub(cl2),
              ball(new Ball(0.4))     // don't forget the ball
        {
        }

        MatchData::~MatchData()
        {
        }

/*
        void MatchData::addPlayer(const std::string& clubname, int idnum, const Color& col)
        {
            if(idnum < 1) 
            {
                throw "MatchData::addPlayer: invalid parameter (idnum)";
            }
            typedef std::pair<std::string, boost::shared_ptr<MatchClub> > pair_cl;
            BOOST_FOREACH(pair_cl pcl, clubs)
            {
                if(pcl.second->getName() == clubname)
                {
                    int n = pcl.second->getNumberOfPlayers();
                    boost::shared_ptr<MatchPlayer> pl (new MatchPlayer(Player("", n, idnum), col));
                    pcl.second->addMatchPlayer(pl);
                    entities.insert(pl);
                    return;
                }
            }
        }
*/
/*
        std::set <boost::shared_ptr<Entity> >* MatchData::getEntities ()
        {
            return &entities;
        }
*/
/*
        void MatchData::updateAll(float interval)
        {
            BOOST_FOREACH(boost::shared_ptr<Entity> d, entities)
            {
                d->update(interval);
            }
        }

        void MatchData::interpolateAll(boost::posix_time::ptime pt)
        {
            BOOST_FOREACH(boost::shared_ptr<Entity> d, entities)
            {
                d->interpolate(pt);
            }
        }
*/

/*
        void MatchData::setHomeLineup(const boost::shared_ptr<Lineup> l)
        {
            homelineup = l;
        }

        void MatchData::setAwayLineup(const boost::shared_ptr<Lineup> l)
        {
            awaylineup = l;
        }
*/

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
/*
        void MatchData::getHomeLineup(boost::shared_ptr<Lineup>& s) const
        {
            s = homelineup;
        }

        void MatchData::getAwayLineup(boost::shared_ptr<Lineup>& s) const
        {
            s = awaylineup;
        }
*/
    }
}
