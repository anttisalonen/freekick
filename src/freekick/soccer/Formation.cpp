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
            float defdiff = 1.0f / (def + 1);
            float middiff = 1.0f / (midf + 1);
            float fordiff = 1.0f / (forw + 1);
            float defhei = 0.25f;
            float midhei = 0.55f;
            float forhei = 0.8f;
            float defwidvar_2 = 0.25f;
            float midwidvar_2 = 0.25f;
            float forwidvar_2 = 0.25f;
            float defheivar_2 = 0.2f;
            float midheivar_2 = 0.15f;
            float forheivar_2 = 0.15f;
            float overlapping_width = 1.05f;

            using addutil::Square;
            using std::pair;
            PlayerMap::const_iterator it;
            for(it = pm.begin(); it != pm.end(); it++)
            {
                switch(it->first)
                {
                    case Goalkeeper:
                        mFormationMap.insert(pair<int, Square>(it->second, Square(0.4f, 0.6f, 0.0f, 0.05f)));
                        break;
                    case Defender:
                        mFormationMap.insert(pair<int, Square>(it->second, 
                                                               Square(defdiff * (defs + 1) - (defdiff * overlapping_width),
                                                                      defdiff * (defs + 1) + (defdiff * overlapping_width),
                                                                      defhei - defheivar_2,
                                                                      defhei + defheivar_2)));
                        defs++;
                        break;
                    case Midfielder:
                        mFormationMap.insert(pair<int, Square>(it->second, 
                                                               Square(middiff * (midfs + 1) - (middiff * overlapping_width),
                                                                      middiff * (midfs + 1) + (middiff * overlapping_width),
                                                                      midhei - midheivar_2,
                                                                      midhei + midheivar_2)));
                        midfs++;
                        break;
                    case Forward:
                        mFormationMap.insert(pair<int, Square>(it->second, 
                                                               Square(fordiff * (forws + 1) - (fordiff * overlapping_width),
                                                                      fordiff * (forws + 1) + (fordiff * overlapping_width),
                                                                      forhei - forheivar_2,
                                                                      forhei + forheivar_2)));
                        forws++;
                        break;
                    default:
                        break;
                }
            }
            return true;
        }

        const addutil::Square& Formation::getPlayerArea(int id) const
        {
            FormationMap::const_iterator it = mFormationMap.find(id);
            if(it == mFormationMap.end()) throw "Formation::getPlayerArea: player not found\n";
            return it->second;
        }

        const addutil::Vector3 Formation::getPlayerAreaCenter(int id) const
        {
            return getPlayerArea(id).getCenter();
        }
    }
}
