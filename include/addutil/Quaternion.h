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


#ifndef QUATERNION_H
#define QUATERNION_H

#include <cmath>

#include <boost/serialization/serialization.hpp>

#include "Vector3.h"

/**
 * class Quaternion
 */

namespace addutil
{
    class Quaternion
    {
    public:
        Quaternion(float _w = 1.0f, float _x = 0.0f, float _y = 0.0f, float _z = 0.0f);
        virtual ~Quaternion() { }
        void set(float _w, float _x, float _y, float _z);
        // void fromVector(const Vector3& n);
        // void fromVector(float _x, float _y, float _z);
        // Vector3 toVector();
        float w;
        float x;
        float y;
        float z;

        friend class boost::serialization::access;
        template<class Archive>
            void serialize(Archive & ar, const unsigned int version)
        {
            ar & w;
            ar & x;
            ar & y;
            ar & z;
        }
    };
}

#endif // QUATERNION_H
