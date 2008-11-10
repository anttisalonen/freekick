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


#ifndef VECTOR3_H
#define VECTOR3_H

#include <cmath>

/**
  * class Vector3
  */

class Vector3
{
public:

    // Constructors/Destructors
    //  
    Vector3();
    /**
     * @param  _x
     * @param  _y
     * @param  _z
     */
    Vector3 (float _x, float _y, float _z );

    Vector3& operator+=(Vector3 a);
    Vector3 operator+(Vector3 a);
    Vector3& operator-=(Vector3 a);
    Vector3 operator-(Vector3 a);
    template <typename T> Vector3& operator*=(T s);
    Vector3 operator*(float s);

    void normalize();
    float length();
    // Public attributes
    //  

    float x;
    float y;
    float z;

};

#endif // VECTOR3_H
