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


#ifndef FREEKICK_SOCCER_TACTIC_H
#define FREEKICK_SOCCER_TACTIC_H

#include <string>

#include <boost/array.hpp>

#include "addutil/XML.h"
#include "addutil/Square.h"

#include "Player.h"

namespace freekick
{
    namespace soccer
    {
        class Tactic
        {
        public:
            Tactic(const std::string& name);
            Tactic(xmlNodePtr root);
            virtual ~Tactic() { }
            const std::string& getName() const;
            const addutil::Vector3& getPosition() const;
            float getOffensive() const;
            void setPosition(const addutil::Vector3& pos);
            void setName(const std::string& name);
            void setOffensive(float off);
        private:
            std::string m_name;
            addutil::Vector3 m_pos;
            float m_offensive;
        };
        Tactic goalkeeperTactic();
    }
}

#endif
