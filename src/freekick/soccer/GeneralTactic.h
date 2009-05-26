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

  Copyright Antti Salonen, 2009
**************************************************************************/


#ifndef GENERALTACTIC_H
#define GENERALTACTIC_H

#include "addutil/XML.h"

namespace freekick
{
    namespace soccer
    {
        class GeneralTactic
        {
        public:
            GeneralTactic();
            GeneralTactic(xmlNodePtr root)
            {
                using namespace addutil::xml;
                xmlNode *node = NULL;
                for (node = root->children; node; node = node->next)
                {
                    if(node_is_node(node, "longball"))
                    {
                        get_attribute(node, "value", long_ball);
                    }
                    else if(node_is_node(node, "widthatt"))
                    {
                        get_attribute(node, "value", width_attack);
                    }
                    else if(node_is_node(node, "widthdef"))
                    {
                        get_attribute(node, "value", width_defense);
                    }
                    else if(node_is_node(node, "countatt"))
                    {
                        get_attribute(node, "value", counter_attack);
                    }
                }
            }
            virtual ~GeneralTactic() { }
            float long_ball;
            float width_attack;
            float width_defense;
            float counter_attack;
        };
    }
}

#endif
