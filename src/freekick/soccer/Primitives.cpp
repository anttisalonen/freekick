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

#include "Primitives.h"

namespace freekick
{
    namespace soccer
    {
        BallOwner other(BallOwner b)
        {
            if(b == Home) return Away;
            return Home;
        }

        soccer::PlayerTarget other(soccer::PlayerTarget b)
        {
            if(b == DownTarget) return UpTarget;
            return DownTarget;
        }

        GoalQuery other(GoalQuery q)
        {
            if(q == HomeGoal) return AwayGoal;
            if(q == AwayGoal) return HomeGoal;
            return q;
        }

        BallOwner intToBallOwner(int n)
        { 
            if(n) return Away; return Home; 
        }

        void setPositionSide(PlayerTarget t, addutil::Vector3& pos, float pitch_width, float pitch_length)
        {
            if(t == DownTarget) return;
            pos.x = pitch_width - pos.x;
            pos.z = pitch_length - pos.z;
        }

        void setPositionSide(BallOwner b, bool secondhalf, addutil::Vector3& pos, float pitch_width, float pitch_length)
        {
            PlayerTarget t = ballOwnerToPlayerTarget(b, secondhalf);
            return setPositionSide(t, pos, pitch_width, pitch_length);
        }

        PlayerTarget ballOwnerToPlayerTarget(BallOwner b, bool secondhalf)
        {
            if((b == Home && !secondhalf) || (b == Away && secondhalf)) 
                return DownTarget;
            return UpTarget;
        }

        BallOwner playerTargetToBallOwner(PlayerTarget t, bool secondhalf)
        {
            if((t == DownTarget && !secondhalf) || (t == UpTarget && secondhalf))
                return Home;
            return Away;
        }

        bool ownersGoal(BallOwner b, GoalQuery g)
        {
            if(g == NoGoal) return false;
            if(b == Home && g == HomeGoal) return true;
            if(b == Away && g == AwayGoal) return true;
            return false;
        }
    }
}
