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
            PlayerPosition pos;
            float active;
            float risktaking;
            float offensive;
            void setArea(bool own, bool offensive, const addutil::Square& sq);
            const addutil::Square& getArea(bool own, bool offensive) const;
            const std::string& getName() const;
        private:
            std::string m_name;
            boost::array<addutil::Square, 4> m_areas;
        };

        int ownOffensiveToIndex(bool own, bool offensive);
    }
}

#endif
