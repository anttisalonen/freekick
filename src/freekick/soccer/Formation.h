/************************************************************************
  This file is part of Freekick.

  Freekick is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  Freekick is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with Freekick.  If not, see <http://www.gnu.org/licenses/>.

  Copyright Antti Salonen, 2008
**************************************************************************/


#ifndef FREEKICKSOCCERFORMATION_H
#define FREEKICKSOCCERFORMATION_H

#include <map>
#include <iostream>

#include <boost/shared_ptr.hpp>

#include "addutil/Circle.h"

#include "Lineup.h"

namespace freekick
{
    namespace soccer
    {
        typedef std::map<int, addutil::Circle> FormationMap;
        class Formation
        {
        public:
            Formation();
            Formation(const boost::shared_ptr<Lineup>& l);
            bool updateLineup(const boost::shared_ptr<Lineup>& l);
            virtual ~Formation() { }
            const addutil::Circle& getPlayerArea(int id) const;
        private:
            FormationMap mFormationMap;
        };
    }
}

#endif
