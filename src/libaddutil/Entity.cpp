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

#include "Entity.h"

Entity::Entity(float _mass)
    : mass(_mass)
{
}

void Entity::update(int v, float x, float y, float z)
{
    switch(v)
    {
        case 0:
            setPosition(x, y, z);
            break;
        case 1:
            setVelocity(x, y, z);
            break;
        case 2:
            setAcceleration(x, y, z);
            break;
        default:
            break;
    }
}
