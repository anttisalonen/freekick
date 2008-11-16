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

#include "Club.h"

namespace freekick
{
    namespace match
    {
/**
 * @param  name
 */
        Club::Club (const std::string& _name ) 
            : name(_name)
        {

        }

        const std::string& Club::getName()
        {
            return name;
        }

        int Club::getNumberOfPlayers()
        {
            return players.size();
        }

        void Club::addPlayer(boost::shared_ptr<Player> p)
        {
            int id = p->getID();
            players[id] = p;
        }

        bool Club::hasPlayer(int i)
        {
            return (players.find(i) != players.end());
        }

        bool Club::updatePlayer(int i, int v, float x, float y, float z)
        {
            std::map <int, boost::shared_ptr<Player> >::iterator it = players.find(i);
            if(it != players.end())
            {
                it->second->update(v, x, y, z);
                return true;
            }
            return false;
        }

        template <typename ContT>
        void Club::getPlayerIDs(ContT& ids)
        {
            ids.clear();
            typedef std::pair<int, boost::shared_ptr<Player> > pair_pl;

            BOOST_FOREACH(pair_pl p, players)
            {
                ids.push_back(p.first);
            }
        }
    }
}
