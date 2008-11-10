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


#ifndef PITCH_H
#define PITCH_H

#include <string>
#include "Drawable.h"
#include "Vector3.h"
#include "Color.h"

/**
  * class Pitch
  */

class Pitch : public Drawable
{
public:

    /**
     * @param  w
     * @param  l
     */
     Pitch (float w, float l );
     const int getID() { return -1002; }

private:

    // Private attributes
    //  

    const float width;
    const float length;

};

#endif // PITCH_H
