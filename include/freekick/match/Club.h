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


#ifndef CLUB_H
#define CLUB_H

#include <string>
#include <vector>
#include <map>

#include <boost/shared_ptr.hpp>
#include <boost/foreach.hpp>

#include "freekick/match/Player.h"
#include "freekick/soccer/Kit.h"

/**
 * class Club
 */

namespace freekick
{
    namespace match
    {
        using namespace freekick::soccer;
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
            bool updatePlayer(int i, int v, float x, float y, float z);
            template <typename ContT> void getPlayerIDs(ContT& ids);

        private:
            std::string name;
            std::map <int, boost::shared_ptr<Player> > players;
            std::vector <Kit> kits;
            Kit* currkit;
        };
    }
}

#endif // CLUB_H
