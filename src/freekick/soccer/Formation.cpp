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

            mTacticList.clear();
            for (node = root->children; node; node = node->next)
            {
                if(node_is_node(node, "lineup"))
                {
                    mLineup.reset(new Lineup(node));
                }
                else if(node_is_node(node, "generaltactic"))
                {
                    mGeneralTactic.reset(new GeneralTactic(node));
                }
                else if(node_is_node(node, "pitchtactic"))
                {
                    xmlNode *pnode = NULL;
                    for (pnode = node->children; pnode; pnode = pnode->next)
                    {
                        if(node_is_node(pnode, "tactic"))
                        {
                            boost::shared_ptr<Tactic> t(new Tactic(pnode));
                            const std::string& tname = t->getName();
                            mTacticList[tname] = t;
                        }
                    }
                    boost::shared_ptr<Tactic> t(new Tactic(goalkeeperTactic()));
                    mTacticList[t->getName()] = t;
                }
            }
        }

        addutil::Vector3 Formation::getPitchPoint(const std::string& pos) const
        {
            TacticList::const_iterator it = mTacticList.find(pos);
            if(it != mTacticList.end())
                return it->second->getPosition();
            throw addutil::Exception("Position not found in tactic list");
        }
        
        boost::shared_ptr<Lineup> Formation::getLineup() const
        {
            return mLineup;
        }

        void Formation::setLineup(boost::shared_ptr<Lineup> l)
        {
            mLineup = l;
        }
    }
}
