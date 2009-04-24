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


#ifndef ADDUTIL_STEERING_H
#define ADDUTIL_STEERING_H

#include <cmath>

#include "Vector3.h"

namespace addutil
{
    namespace steering
    {
        Vector3 seek(const Vector3& from, const Vector3& to, float maxspeed);
        Vector3 arrive(const Vector3& from, const Vector3& to, float maxspeed);
        Vector3 pursuit(const Vector3& from, const Vector3& to, const Vector3& tgtvelocity, float maxspeed);
    }
}

#endif
