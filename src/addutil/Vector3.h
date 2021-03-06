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


#ifndef VECTOR3_H
#define VECTOR3_H

#include <cmath>

#include <boost/serialization/serialization.hpp>

/**
 * class Vector3
 */

namespace addutil
{
    enum Axis
    {
        X_Axis,
        Y_Axis,
        Z_Axis
    };

    static const float pi = 3.1415926535f;
    static const float pi_2 = pi / 2.0f;
    static const float pi_4 = pi / 4.0f;
    static const float pi_3_4 = 3.0f * pi / 4.0f;
    static const float pi_8 = pi / 8.0f;

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

        Vector3& operator+=(const Vector3& a);
        Vector3 operator+(const Vector3& a) const;
        Vector3& operator-=(const Vector3& a);
        Vector3 operator-(const Vector3& a) const;
        Vector3& operator*=(float s);
        Vector3 operator*(float s) const;
        bool operator==(const Vector3& b) const;

        Vector3& normalize();
        float length() const;
        float length2() const;
        void capX(float n);
        void capY(float n);
        void capZ(float n);
        void reset();
        void set(float _x, float _y, float _z);
        Vector3 normalized() const;
        float dot(const Vector3& b) const;
        Vector3 cross(const Vector3& b) const;
        float angleBetweenXZ(const Vector3& b) const;

        float XZAngle() const;
        // Public attributes
        //  

        float x;
        float y;
        float z;

        friend class boost::serialization::access;
        template<class Archive>
            void serialize(Archive & ar, const unsigned int version)
        {
            ar & x;
            ar & y;
            ar & z;
        }
    };

    std::ostream& operator<<(std::ostream& os, const Vector3& v);
    bool inArea(const Vector3& tl, const Vector3& br, const Vector3& p);
}

#endif // VECTOR3_H
