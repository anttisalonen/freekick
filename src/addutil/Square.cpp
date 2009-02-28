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

#include "Square.h"

namespace addutil
{
    Square::Square(float minx, float maxx, float minz, float maxz)
    {
        topleft.x = minx;
        bottomright.x = maxx;
        topleft.z = minz;
        bottomright.z = maxz;
    }

    Square::Square(const Vector3& a, const Vector3& b)
    {
        topleft.x = std::min(a.x, b.x);
        bottomright.z = std::max(a.z, b.z);
    }

    bool Square::in(const Vector3& t) const
    {
        if(t.x < topleft.x) return false;
        if(t.x > bottomright.x) return false;
        if(t.z < topleft.z) return false;
        if(t.z > bottomright.z) return false;
        return true;
    }

    const Vector3 Square::getCenter() const
    {
        return (Vector3((bottomright.x - topleft.x) / 2.0f + topleft.x, 0.0f, (bottomright.z - topleft.z) / 2.0f + topleft.z));
    }
}
