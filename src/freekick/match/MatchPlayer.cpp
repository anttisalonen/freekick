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
  This file was generated on So Okt 26 2008 at 12:09:20
**************************************************************************/

#include "MatchPlayer.h"

namespace freekick
{
    namespace match
    {
        MatchPlayer::MatchPlayer (const freekick::soccer::Player& p, soccer::BallOwner _side, bool sub)
            : Player(p), 
              DynamicEntity(70.0f, "robot.mesh"),
              m_side(_side),
              substitute(sub)
        {
        }

        const int MatchPlayer::getID() const
        {
            return Player::getID();
        }

        bool MatchPlayer::isSubstitute() const
        {
            return substitute;
        }

        soccer::BallOwner MatchPlayer::getSide() const
        {
            return m_side;
        }
    }
}
