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


#ifndef CLUB_H
#define CLUB_H

#include <string>
#include <vector>
#include <map>

#include <boost/shared_ptr.hpp>
#include <boost/foreach.hpp>
#include <boost/serialization/string.hpp>
#include <boost/serialization/map.hpp>
#include <boost/serialization/vector.hpp>

#include "freekick/soccer/Player.h"
#include "freekick/soccer/Kit.h"

/**
 * class Club
 */

namespace freekick
{
    namespace soccer
    {
        class Club
        {
        public:
            /**
             * @param  name
             */
            Club (const std::string& _name);
            const std::string& getName();
            int getNumberOfPlayers();
            void addPlayer(boost::shared_ptr<Player> p);
            bool hasPlayer(int i);
            const Player& getPlayer(int i);
            void getPlayers(std::vector<boost::shared_ptr<Player> >& pls);
            void getPlayerIDs(std::vector<int>& ids);

        private:
            std::string name;
            std::map <int, boost::shared_ptr<Player> > players;
            std::vector <Kit> kits;

            friend class boost::serialization::access;
            template<class Archive>
                void serialize(Archive & ar, const unsigned int version)
            {
                ar & name;
                ar & players;
                ar & kits;
            }
        };
    }
}

#endif // CLUB_H
