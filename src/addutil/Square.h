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


#ifndef ADDUTIL_SQUARE_H
#define ADDUTIL_SQUARE_H

#include <algorithm>
#include <iostream>

#include "Vector3.h"

namespace addutil
{
    class Square
    {
    public:
        Square(float minx, float maxx, float minz, float maxz);
        Square(const Vector3& a, const Vector3& b);
        bool in(const Vector3& t) const;
        const Vector3 getCenter() const;        
        virtual ~Square() { }
    private:
        Vector3 topleft;
        Vector3 bottomright;
    };
}

#endif
