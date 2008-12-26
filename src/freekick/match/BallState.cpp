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

#include "BallState.h"

namespace freekick
{
    namespace match
    {
        BallState::BallState ()
        {
            bio_type = PreKickoff; blocked_play = true; 
        }

        void BallState::flipOwner()
        {
            owner = other(owner);
        }

        BallInOut intToBallInOut(int n)
        {
            switch(n)
            {
                case 0: return BallIn;
                case 1: return PreKickoff;
                case 2: return Kickoff;
                case 3: return Throwin;
                case 4: return Goalkick;
                case 5: return Cornerkick;
                case 6: return IndirectFreekick;
                case 7: return DirectFreekick;
                case 8: return PenaltyKick;
                case 9: return DroppedBall;
                default: break;
            }
            return HalfFullTime;
        }

        BallOwner other(BallOwner b)
        {
            if(b == Home) return Away;
            return Home;
        }

        BallOwner intToBallOwner(int n)
        { 
            if(n) return Away; return Home; 
        }
    }
}
