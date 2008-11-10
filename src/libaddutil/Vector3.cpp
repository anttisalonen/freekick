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

#include "Vector3.h"

// Constructors/Destructors
//  

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

void Vector3::normalize()
{
    float len = length();
    x = x / len;
    y = y / len;
    z = z / len;
}

float Vector3::length()
{
    return sqrt(x * x + y * y + z * z);
}

Vector3& Vector3::operator+=(Vector3 a)
{
    x += a.x;
    y += a.y;
    z += a.z;
    return *this;
}

Vector3 Vector3::operator+(Vector3 a)
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

Vector3 Vector3::operator-(Vector3 a)
{
    Vector3 r(x - a.x, y - a.y, z - a.z);
    return r;
}

template <typename T> Vector3& Vector3::operator*=(T s)
{
    x *= s;
    y *= s;
    z *= s;
    return *this;
}

Vector3 Vector3::operator*(float s)
{
    Vector3 r(x * s, y * s, z * s);
    return r;
}

