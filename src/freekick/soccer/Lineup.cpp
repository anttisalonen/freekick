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

#include "Lineup.h"

namespace freekick
{
    namespace soccer
    {
        Lineup::Lineup()
        {
        }

        bool Lineup::doSubstitution(int out, int in)
        {
            // TODO
            return false;
        }

        PlayerInLineup Lineup::playerInLineup(int plid) const
        {
            PlayerMap::const_iterator it1, it2;
            for(it1 = pitchplayers.begin(); it1 != pitchplayers.end(); it1++)
            {
                if((*it1).second == plid) return Playing;
            }
            for(it2 = substitutes.begin(); it2 != substitutes.end(); it2++)
            {
                if((*it2).second == plid) return Substitute;
            }
            return NotPlaying;
        }

        PlayerPosition Lineup::getPlayerPosition(int plid) const
        {
            PlayerMap::const_iterator it1;
            for(it1 = pitchplayers.begin(); it1 != pitchplayers.end(); it1++)
            {
                if((*it1).second == plid)
                {
                    return it1->first;
                }
            }
            throw "Lineup::getPlayerPosition: not playing\n";
        }

        std::vector<int> Lineup::getPitchPlayerIDs() const
        {
            std::vector<int> ids;
            PlayerMap::const_iterator it;
            for(it = pitchplayers.begin(); it != pitchplayers.end(); it++)
            {
                ids.push_back(it->second);
            }
            return ids;
        }

        std::vector<int> Lineup::getSubstituteIDs() const
        {
            std::vector<int> ids;
            PlayerMap::const_iterator it;
            for(it = substitutes.begin(); it != substitutes.end(); it++)
            {
                ids.push_back(it->second);
            }
            return ids;
        }

        void Lineup::addPlayer(int id, PlayerPosition pos, bool substitute)
        {
            if(!substitute)
                pitchplayers.insert(std::pair<PlayerPosition, int>(pos, id));
            else
                substitutes.insert(std::pair<PlayerPosition, int>(pos, id));
        }

        const PlayerMap& Lineup::getPitchPlayers() const
        {
            return pitchplayers;
        }

        const PlayerMap& Lineup::getSubstitutes() const
        {
            return substitutes;
        }

        void Lineup::clear()
        {
            pitchplayers.clear();
            substitutes.clear();
        }

        void playersInPlayerMap(const PlayerMap& pm, int& gk, int& def, int& midf, int& forw)
        {
            gk = def = midf = forw = 0;
            PlayerMap::const_iterator it;
            for(it = pm.begin(); it != pm.end(); it++)
            {
                switch(it->first)
                {
                    case Goalkeeper: gk++;   break;
                    case Defender:   def++;  break;
                    case Midfielder: midf++; break;
                    case Forward:    forw++; break;
                    default: break;
                }
            }
        }
    }
}
