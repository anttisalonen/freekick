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

#include "Pitch.h"

namespace freekick
{
    namespace soccer
    {
        Pitch::Pitch (float w, float l ) 
            : width(w), length(l) 
        {
        }

        float Pitch::getWidth() const
        {
            return width;
        }

        float Pitch::getLength() const
        {
            return length;
        }

        bool Pitch::onPitch(float x, float y) const
        {
            if(x < 0.0f || y < 0.0f || x > width || y > length) return false;
            return true;
        }

        bool Pitch::onSide(bool top, float x, float y) const
        {
            if(!onPitch(x, y)) return false;
            bool ontopside = (y < (length / 2.0f));
            if(top) return ontopside;
            return !ontopside;
        }
    }
}


