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

#include "Quaternion.h"

namespace addutil
{
    Quaternion::Quaternion()
    {
    }

    Quaternion::Quaternion (float _w, float _x, float _y, float _z)
        : w(_w)
        , x(_x)
        , y(_y)
        , z(_z)
    {
    }

    Quaternion::Quaternion(float xzangle)
    {
        w = cos(xzangle);
        y = sin(xzangle/2.0f);
    }

    void Quaternion::set(float _w, float _x, float _y, float _z)
    {
        w = _w;
        x = _x;
        y = _y;
        z = _z;
    }

    void Quaternion::toAxisAngle(addutil::Vector3& axis, float& angle) const
    {
        angle = 2 * acos(w);
        float l = sqrt(x * x + y * y + z * z);
        if(l > 0.0f)
        {
            axis.set(x / l, y / l, z / l);
        }
        else
        {
            axis.reset();
        }
    }

/*
    Vector3 Quaternion::toVector()
    {
        // float yaw   = atan2f(2.0f * (w * x + y * z), 1 - 2.0f * (y * y + z * z));
        // float pitch = asinf(2.0f * (w * y - z * x));
        // TODO: verify
        return Vector3(2.0f * (w * x + y * z), 1 - 2.0f * (y * y + z * z), 2.0f * (w * y - z * x));
    }

    void Quaternion::fromVector(const Vector3& n)
    {
        Vector3 m = n;
        m.normalize();
        // float yaw   = atan2(m.y, m.x);
        // float pitch = asinf(m.z);
        float ycos  = m.x / 2.0f; // cosf(yaw / 2.0f);
        float ysin  = m.y; // sinf(yaw / 2.0f);
        float pcos  = 

        n.normalize();
        s = 
        w = 0.5;
        x = n.x * s;
    }

    void Quaternion::fromVector(float _x, float _y, float _z)
    {
        fromVector(Vector3(_x, _y, _z));
    }
*/

    std::ostream& operator<<(std::ostream& os, const Quaternion& e)
    {
        os << e.w << " " << e.x << " " << e.y << " " << e.z;
        return os;
    }
}
