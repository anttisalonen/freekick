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

#include "MatchStatus.h"

namespace freekick
{
    namespace match
    {
// Constructors/Destructors
//  

/**
 */
        MatchStatus::MatchStatus ( ) 
            : ball(new MatchBall(0.4f))
        {
            entities.insert(ball);
        }

        MatchStatus::~MatchStatus()
        {
        }

/**
 * @param  evt
 */
        void MatchStatus::newEvent (const std::string& evt ) 
        {
            int n, v;
            float x, y, z, a, b, g;
            std::istringstream ist(evt);
            ist >> n >> v >> x >> y >> z >> a >> b >> g;
            if(n == -2)
            {
                ball->update(v, x, y, z);
                return;
            }
            typedef std::pair<std::string, boost::shared_ptr<MatchClub> > pair_cl;
            BOOST_FOREACH(pair_cl cl, clubs)
            {
                if (cl.second->updatePlayer(n, v, x, y, z))
                    return;
            }
        }


/**
 * @param  events
 */
        void MatchStatus::newEvents (std::vector <std::string>& events ) 
        {
            BOOST_FOREACH(std::string s, events)
                newEvent(s);
        }

        void MatchStatus::addClub(const std::string& name)
        {
            boost::shared_ptr<MatchClub> cl (new MatchClub(name));
            clubs[name] = cl;
            int numclubs = clubs.size();
            if(numclubs == 1) 
                homeclub = cl;
            else
                awayclub = cl;
        }

        void MatchStatus::addPlayer(const std::string& clubname, int idnum, const Color& col)
        {
            if(idnum < 1) 
            {
                throw "MatchStatus::addPlayer: invalid parameter (idnum)";
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

        std::set <boost::shared_ptr<Entity> >* MatchStatus::getEntities ()
        {
            return &entities;
        }

        void MatchStatus::updateAll(float interval)
        {
            BOOST_FOREACH(boost::shared_ptr<Entity> d, entities)
            {
                d->update(interval);
            }
        }

        void MatchStatus::interpolateAll(boost::posix_time::ptime pt)
        {
            BOOST_FOREACH(boost::shared_ptr<Entity> d, entities)
            {
                d->interpolate(pt);
            }
        }

        bool MatchStatus::run()
        {
            /*
              long milliseconds = 25;
              while(1)
              {
              boost::posix_time::ptime start_time = boost::posix_time::microsec_clock::universal_time();
              updateAll(milliseconds / 1000.0f);
              boost::posix_time::ptime end_time = boost::posix_time::microsec_clock::universal_time();
              boost::posix_time::time_period diff_time(start_time, end_time);
              boost::posix_time::time_duration diff_dur = diff_time.length();
              long ms_diff = diff_dur.total_milliseconds();

              if(ms_diff > milliseconds)
              {
              milliseconds++;
              }
              else
              {
              boost::this_thread::sleep(boost::posix_time::milliseconds(milliseconds - ms_diff));
              }
              }
            */
            return true;
        }

/*
  const std::string& MatchStatus::getHomeClubName() const
  {
  if(clubs.size() == 0) throw "MatchStatus::getHomeClubName: No clubs in status";
  std::map<std::string, boost::shared_ptr<Club> >::iterator it = clubs.begin();
  return (*it).first;
  }
*/

        template <typename ContT>
        void MatchStatus::getHomePlayerIDs(ContT& ids)
        {
            if(homeclub.use_count() == 0) throw "MatchStatus::getHomePlayerIDs: No home club";
            getClubPlayerIDs(homeclub, ids);
        }

        template <typename ContT>
        void MatchStatus::getAwayPlayerIDs(ContT& ids)
        {
            if(awayclub.use_count() == 0) throw "MatchStatus::getAwayPlayerIDs: No away club";
            getClubPlayerIDs(awayclub, ids);
        }

        template <typename ContT>
        void MatchStatus::getClubPlayerIDs(const boost::shared_ptr<MatchClub> c, ContT& ids)
        {
            c->getPlayerIDs(ids);
        }
    }
}
