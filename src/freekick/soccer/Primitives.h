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


#ifndef FREEKICK_PRIMITIVES_H
#define FREEKICK_PRIMITIVES_H

#include "addutil/Vector3.h"

namespace freekick
{
    namespace soccer
    {
        enum BallOwner
        {
            Home,
            Away
        };

        enum GoalQuery
        {
            NoGoal,
            HomeGoal,
            AwayGoal
        };

        enum PlayerTarget
        {
            NoTarget,
            DownTarget,
            UpTarget
        };

        // TODO: add pitch measurements from pitch.h, rules.cpp here

        BallOwner other(BallOwner b);
        PlayerTarget other(PlayerTarget b);
        GoalQuery other(GoalQuery q);
        BallOwner intToBallOwner(int n);
        void setPositionSide(PlayerTarget t, addutil::Vector3& pos, float pitch_width, float pitch_length);
        void setPositionSide(BallOwner b, bool secondhalf, addutil::Vector3& pos, float pitch_width, float pitch_length);
        PlayerTarget ballOwnerToPlayerTarget(BallOwner b, bool secondhalf);
        BallOwner playerTargetToBallOwner(PlayerTarget t, bool secondhalf);
        bool ownersGoal(BallOwner b, GoalQuery g);
    }
}

#endif
