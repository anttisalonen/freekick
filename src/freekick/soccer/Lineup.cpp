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

#include "Lineup.h"

namespace freekick
{
    namespace soccer
    {
        Lineup::Lineup()
        {
        }

        Lineup::Lineup(xmlNodePtr root)
        {
            using namespace addutil::xml;
            xmlNode *node = NULL;
            std::string name;

            for (node = root->children; node; node = node->next)
            {
                if(node_is_node(node, "position"))
                {
                    std::string name;
                    int plid;
                    get_attribute(node, "player", plid);
                    get_attribute(node, "name", name);
                    pitchplayers[plid] = name;
                }
                else if(node_is_node(node, "substitute"))
                {
                    int plid;
                    get_attribute(node, "player", plid);
                    substitutes[plid] = "";
                }
            }
        }

        bool Lineup::doSubstitution(int out, int in)
        {
            // TODO
            return false;
        }

        PlayerInLineup Lineup::playerInLineup(int plid) const
        {
            PlayerMap::const_iterator it1, it2;
            it1 = pitchplayers.find(plid);
            if(it1 != pitchplayers.end()) return Playing;
            it2 = substitutes.find(plid);
            if(it2 != substitutes.end()) return Substitute;
            return NotPlaying;
        }

        const std::string& Lineup::getPlayerPosition(int plid) const
        {
            PlayerMap::const_iterator it1;
            it1 = pitchplayers.find(plid);
            if(it1 != pitchplayers.end()) return it1->second;
            throw "Lineup::getPlayerPosition: not playing\n";
        }

        std::vector<int> Lineup::getPitchPlayerIDs() const
        {
            std::vector<int> ids;
            PlayerMap::const_iterator it;
            for(it = pitchplayers.begin(); it != pitchplayers.end(); it++)
            {
                ids.push_back(it->first);
            }
            return ids;
        }

        std::vector<int> Lineup::getSubstituteIDs() const
        {
            std::vector<int> ids;
            PlayerMap::const_iterator it;
            for(it = substitutes.begin(); it != substitutes.end(); it++)
            {
                ids.push_back(it->first);
            }
            return ids;
        }

        void Lineup::addPlayer(int id, const std::string& pos, bool substitute)
        {
            if(!substitute)
                pitchplayers.insert(std::pair<int, const std::string>(id, pos));
            else
                substitutes.insert(std::pair<int, const std::string>(id, pos));
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
    }
}
