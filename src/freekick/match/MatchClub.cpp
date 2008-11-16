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

#include "MatchClub.h"

namespace freekick
{
    namespace match
    {
/**
 * @param  name
 */
        MatchClub::MatchClub (const Club& c)
            : Club(c)
        {
        }

        void MatchClub::addMatchPlayer(boost::shared_ptr<MatchPlayer> p)
        {
            int id = p->getID();
            matchplayers[id] = p;
        }

        bool MatchClub::updatePlayer(int i, int v, float x, float y, float z)
        {
            std::map <int, boost::shared_ptr<MatchPlayer> >::iterator it = matchplayers.find(i);
            if(it != matchplayers.end())
            {
                it->second->update(v, x, y, z);
                return true;
            }
            return false;
        }
    }
}
