/************************************************************************
  This file is part of Freekick.

  Freekick is free software: you can redistribute it and/or modify
  it under the terms of the GNU Affero General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  Freekick is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU Affero General Public License for more details.

  You should have received a copy of the GNU Affero General Public License
  along with Freekick.  If not, see <http://www.gnu.org/licenses/>.

  Copyright Antti Salonen, 2008
**************************************************************************/

#include "Formation.h"

namespace freekick
{
    namespace soccer
    {
        Formation::Formation()
        {
        }

        Formation::Formation(const boost::shared_ptr<Lineup>& l)
        {
            if(!updateLineup(l)) throw "Formation::Formation: invalid lineup";
        }

        bool Formation::updateLineup(const boost::shared_ptr<Lineup>& l)
        {
            int goalk, def, midf, forw;
            const PlayerMap pm = l->getPitchPlayers();
            playersInPlayerMap(pm, goalk, def, midf, forw);
            std::cerr << "Formation::updateLineup: goalk: " << goalk << " def: " << def << " midf: " << midf << " forw: " << forw << std::endl;
            if(goalk != 1 || 
               (def + midf + forw != 10) || 
               def < 3 || 
               def > 6 || 
               midf < 1 || 
               midf > 5 || 
               forw < 1 || 
               forw > 5) 
                return false;

            int defs = 0, midfs = 0, forws = 0;
            float defdiff = 1.0f / def;
            float middiff = 1.0f / midf;
            float fordiff = 1.0f / forw;
            float defhei = 0.25f;
            float midhei = 0.5f;
            float forhei = 0.7f;

            using addutil::Circle;
            using std::pair;
            PlayerMap::const_iterator it;
            for(it = pm.begin(); it != pm.end(); it++)
            {
                switch(it->first)
                {
                    case Goalkeeper:
                        mFormationMap.insert(pair<int, Circle>(it->second, Circle(0.5f, 0.1f, 0.1f)));
                        break;
                    case Defender:
                        mFormationMap.insert(pair<int, Circle>(it->second, Circle(defdiff * defs + defdiff / 2.0f, defhei, defdiff)));
                        defs++;
                        break;
                    case Midfielder:
                        mFormationMap.insert(pair<int, Circle>(it->second, Circle(middiff * midfs + middiff / 2.0f, midhei, middiff)));
                        midfs++;
                        break;
                    case Forward:
                        mFormationMap.insert(pair<int, Circle>(it->second, Circle(fordiff * forws + fordiff / 2.0f, forhei, fordiff)));
                        forws++;
                        break;
                    default:
                        break;
                }
            }
            return true;
        }

        const addutil::Circle& Formation::getPlayerArea(int id) const
        {
            FormationMap::const_iterator it = mFormationMap.find(id);
            if(it == mFormationMap.end()) throw "Formation::getPlayerArea: player not found\n";
            return it->second;
        }
    }
}
