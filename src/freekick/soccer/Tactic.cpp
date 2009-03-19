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

#include "Tactic.h"

namespace freekick
{
    namespace soccer
    {
        Tactic::Tactic(const std::string& name)
            : m_name(name)
        {
        }

        Tactic::Tactic(xmlNodePtr root)
        {
            using namespace addutil::xml;
            xmlNode *node = NULL;
            // std::cout << "getting node named " << root->name << std::endl;
            get_attribute(root, "name", m_name);

            for (node = root->children; node; node = node->next)
            {
                if(node_is_node(node, "position"))
                {
                    pos = pp_from_xml(node);
                }
                else if(node_is_node(node, "attributes"))
                {
                    // TODO
                }
                else if(node_is_node(node, "area"))
                {
                    int own, offensive;
                    float min_x, max_x, min_y, max_y;
                    get_attribute(node, "own", own);
                    get_attribute(node, "offensive", offensive);
                    get_attribute(node, "min_x", min_x);
                    get_attribute(node, "min_y", min_y);
                    get_attribute(node, "max_x", max_x);
                    get_attribute(node, "max_y", max_y);
                    int index = ownOffensiveToIndex(own, offensive);
                    m_areas[index] = addutil::Square(min_x, max_x, min_y, max_y);
                }
            }
        }

        void Tactic::setArea(bool own, bool offensive, const addutil::Square& sq)
        {
            int index = ownOffensiveToIndex(own, offensive);
            m_areas[index] = sq;
        }

        const addutil::Square& Tactic::getArea(bool own, bool offensive) const
        {
            int index = ownOffensiveToIndex(own, offensive);
            return m_areas[index];
        }

        const std::string& Tactic::getName() const
        {
            return m_name;
        }

        int ownOffensiveToIndex(bool own, bool offensive)
        {
            int index = 0;
            if(own) index += 1;
            if(offensive) index += 2;
            return index;            
        }
    }
}
