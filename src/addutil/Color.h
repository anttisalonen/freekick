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


#ifndef COLOR_H
#define COLOR_H

#include <string>

#include <boost/serialization/serialization.hpp>

/**
  * class Color
  */

namespace addutil
{
    class Color
    {
    public:

        Color(float r = 0.0f, float g = 0.0f, float b = 0.0f);
        Color(int r, int g, int b);
        float red;
        float green;
        float blue;
        void  toIntStream(std::ostream& s) const;

        friend class boost::serialization::access;
        template<class Archive>
            void serialize(Archive & ar, const unsigned int version)
        {
            ar & red;
            ar & green;
            ar & blue;
        }
    };
    int colorValueToInt(float v);
    float intToColorValue(int v);
    std::ostream& operator <<(std::ostream& os, const Color& c);
}

#endif // COLOR_H
