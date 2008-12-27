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

#include "Player.h"

namespace freekick
{
    namespace soccer
    {
        Player::Player (const std::string& _name, int num, unsigned int _idnumber, PlayerPosition pos)
            : Human(_name),
              number(num), 
              idnumber(_idnumber),
              position(pos)
        {
        }

        unsigned int Player::getID() const
        {
            return idnumber;
        }

        int Player::getNumber() const
        {
            return number;
        }

        PlayerPosition Player::getPlayerPosition() const
        {
            return position;
        }

        PlayerPosition IntToPlayerPosition(int i)
        {
            if(i == 0)
                return Goalkeeper;
            else if (i == 1)
                return Defender;
            else if (i == 2)
                return Midfielder;
            return Forward;
        }
    }
}
