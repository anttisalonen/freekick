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

#include "Vector3.h"

// Constructors/Destructors
//  

namespace addutil
{
    Vector3::Vector3()
    {
        x = 0.0f;
        y = 0.0f;
        z = 0.0f;
    }

/**
 * @param  _x
 * @param  _y
 * @param  _z
 */
    Vector3::Vector3 (float _x = 0.0f, float _y = 0.0f, float _z = 0.0f ) :
        x(_x), y(_y), z(_z) 
    {
    }

    Vector3& Vector3::normalize()
    {
        *this = normalized();
        return *this;
    }

    float Vector3::length() const
    {
        return sqrt(x * x + y * y + z * z);
    }

    float Vector3::length2() const
    {
        return x * x + y * y + z * z;
    }

    void Vector3::capX(float n)
    {
        if(x > n) x = n;
    }

    void Vector3::capY(float n)
    {
        if(y > n) y = n;
    }

    void Vector3::capZ(float n)
    {
        if(z > n) z = n;
    }

    void Vector3::reset()
    {
        x = y = z = 0.0f;
    }

    Vector3 Vector3::normalized() const
    {
        float len = length();
        if(len == 0.0f) return Vector3(0.0f, 0.0f, 0.0f);
        return Vector3(x / len, y / len, z / len);
    }

    void Vector3::set(float _x, float _y, float _z)
    {
        x = _x;
        y = _y;
        z = _z;
    }

    float Vector3::XZAngle() const
    {
        return atan2(-z, x);
    }

    Vector3& Vector3::operator+=(Vector3 a)
    {
        x += a.x;
        y += a.y;
        z += a.z;
        return *this;
    }

    Vector3 Vector3::operator+(Vector3 a) const
    {
        Vector3 r(x + a.x, y + a.y, z + a.z);
        return r;
    }

    Vector3& Vector3::operator-=(Vector3 a)
    {
        x -= a.x;
        y -= a.y;
        z -= a.z;
        return *this;
    }

    Vector3 Vector3::operator-(Vector3 a) const
    {
        Vector3 r(x - a.x, y - a.y, z - a.z);
        return r;
    }

    Vector3& Vector3::operator*=(float s)
    {
        x *= s;
        y *= s;
        z *= s;
        return *this;
    }

    Vector3 Vector3::operator*(float s) const
    {
        Vector3 r(x * s, y * s, z * s);
        return r;
    }

    std::ostream& operator<<(std::ostream& os, const Vector3& v)
    {
        os << v.x << " " << v.y << " " << v.z;
        return os;
    }

    bool inArea(const Vector3& tl, const Vector3& br, const Vector3& p)
    {
        if(p.x < tl.x) return false;
        if(p.z < tl.z) return false;
        if(p.x > br.x) return false;
        if(p.z > br.z) return false;
        return true;
    }
}
