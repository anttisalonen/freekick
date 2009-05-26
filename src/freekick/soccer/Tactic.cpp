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
                if(node_is_node(node, "pos"))
                {
                    float x, y;
                    get_attribute(node, "y", y);
                    get_attribute(node, "x", x);
                    m_pos.set(x, 0.0f, y);
                }
                else if(node_is_node(node, "attributes"))
                {
                    get_attribute(node, "offensive", m_offensive);
                }
            }
        }

        const std::string& Tactic::getName() const
        {
            return m_name;
        }

        const addutil::Vector3& Tactic::getPosition() const
        {
            return m_pos;
        }

        float Tactic::getOffensive() const
        {
            return m_offensive;
        }

        void Tactic::setPosition(const addutil::Vector3& pos)
        {
            m_pos = pos;
        }

        void Tactic::setName(const std::string& name)
        {
            m_name = name;
        }

        void Tactic::setOffensive(float off)
        {
            m_offensive = off;
        }

        Tactic goalkeeperTactic()
        {
            Tactic t("Goalkeeper");
            t.setOffensive(0.05);
            t.setPosition(addutil::Vector3(0.5f, 0.0f, 0.0f));
            return t;
        }
    }
}
