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
  This file was generated on So Okt 26 2008 at 12:09:20
**************************************************************************/


#ifndef GOAL_H
#define GOAL_H

#include <string>

#include "addutil/Vector3.h"
#include "addutil/Color.h"
#include "addutil/StaticEntity.h"

#include "MatchIDs.h"

/**
 * class Goal
 */

namespace freekick
{
    namespace match
    {
        class Goal : public addutil::StaticEntity
        {
        public:
            /**
             * @param  first
             */
            Goal (bool _first );
            const int getID() const { if (first) return FirstGoalID; return SecondGoalID; }

        private:
            bool first;
        };
    }
}

#endif // GOAL_H
