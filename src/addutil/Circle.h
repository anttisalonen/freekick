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


#ifndef ADDUTIlCIRCLE_H
#define ADDUTILCIRCLE_H

#include "Vector3.h"

namespace addutil
{
    class Circle
    {
    public:
        Circle(Vector3 v, float rad);
        Circle(float x, float y, float rad);
        virtual ~Circle() { }
        const Vector3& getCenter() const;
        float getRadius() const;
        bool inCircle(int x, int y) const;
        bool inCircle(Vector3 v) const;
    private:
        Vector3 mLocation;
        float mRadius;
    };
}

#endif
