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

#include "Circle.h"

namespace addutil
{
    Circle::Circle(Vector3 v, float rad)
        : mLocation(v),
          mRadius(rad)
    {
        mLocation.y = 0.0f;
    }

    Circle::Circle(float x, float y, float rad)
        : mLocation(Vector3(x, 0.0f, y)),
          mRadius(rad)
    {
    }

    const Vector3& Circle::getCenter() const
    {
        return mLocation;
    }

    float Circle::getRadius() const
    {
        return mRadius;
    }

    bool Circle::inCircle(int x, int y) const
    {
        return inCircle(Vector3(x, 0.0f, y));
    }

    bool Circle::inCircle(Vector3 v) const
    {
        Vector3 n(v.x, 0.0f, v.z);
        if((n - mLocation).length() <= mRadius) return true;
        return false;
    }
}
