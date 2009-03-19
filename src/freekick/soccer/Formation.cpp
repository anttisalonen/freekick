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

        Formation::Formation(xmlNodePtr root)
        {
            using namespace addutil::xml;
            xmlNode *node = NULL;
            get_attribute(root, "name", m_name);

            mTacticList.clear();
            for (node = root->children; node; node = node->next)
            {
                if(node_is_node(node, "tactic"))
                {
                    boost::shared_ptr<Tactic> t(new Tactic(node));
                    const std::string& tname = t->getName();
                    mTacticList[tname] = t;
                }
            }
        }

        const addutil::Square& Formation::getTacticArea(bool own, bool offensive, const std::string& tactic) const
        {
            TacticList::const_iterator it = mTacticList.find(tactic);
            if(it == mTacticList.end()) throw "Formation::getTacticArea: tactic not found\n";
            return it->second->getArea(own, offensive);
        }

        const addutil::Vector3 Formation::getTacticAreaCenter(bool own, bool offensive, const std::string& tactic) const
        {
            return getTacticArea(own, offensive, tactic).getCenter();
        }

        bool Formation::inTacticArea(bool own, bool offensive, const std::string& tactic, const addutil::Vector3& plloc) const
        {
            return getTacticArea(own, offensive, tactic).in(plloc);
        }
    }
}
