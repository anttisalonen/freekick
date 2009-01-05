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


#ifndef FREEKICK_LINEUP_H
#define FREEKICK_LINEUP_H

#include <vector>
#include <map>

#include <boost/shared_ptr.hpp>
#include <boost/foreach.hpp>

#include "Player.h"

namespace freekick
{
    namespace soccer
    {
        typedef std::multimap<PlayerPosition, int> PlayerMap;

        enum PlayerInLineup
        {
            NotPlaying,
            Playing,
            Substitute
        };

        class Lineup
        {
        public:
            Lineup();
            virtual ~Lineup() { }
            bool doSubstitution(int out, int in);
            PlayerInLineup playerInLineup(int plid) const;
            PlayerPosition getPlayerPosition(int plid) const;
            void addPlayer(int id, PlayerPosition pos, bool substitute);
            void clear();
            std::vector<int> getPitchPlayerIDs() const;
            std::vector<int> getSubstituteIDs() const;
            const PlayerMap& getPitchPlayers() const;
            const PlayerMap& getSubstitutes() const;
        private:
            PlayerMap pitchplayers;
            PlayerMap substitutes;
        };
    }
}

#endif
