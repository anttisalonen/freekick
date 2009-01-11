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
        Vector3 operator+(Vector3 a) const;
        Vector3& operator-=(Vector3 a);
        Vector3 operator-(Vector3 a) const;
        Vector3& operator*=(float s);
        Vector3 operator*(float s) const;

        void normalize();
        float length() const;
        void capX(float n);
        void capY(float n);
        void capZ(float n);
        void reset();

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

    std::ostream& operator<<(std::ostream& os, const Vector3& e);
}

#endif // VECTOR3_H
