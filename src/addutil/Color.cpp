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

#include "Color.h"

// Constructors/Destructors
//

/**
 * @param  _x
 * @param  _y
 * @param  _z
 */

namespace addutil
{
    Color::Color (float r, float g, float b) :
        red(r), green(g), blue(b)
    {
    }

    Color::Color(int r, int g, int b)
    {
        red = intToColorValue(r);
        green = intToColorValue(g);
        blue = intToColorValue(b);
    }

    void Color::toIntStream(std::ostream& s) const
    {
        s << colorValueToInt(red) << " " << colorValueToInt(green) << " " << colorValueToInt(blue);
    }

    int colorValueToInt(float v)
    {
        return v * 255;
    }

    float intToColorValue(int v)
    {
        return v / 255.0f;
    }

    std::ostream& operator <<(std::ostream& os, const Color& c)
    {
        c.toIntStream(os);
        return os;
    }
}
