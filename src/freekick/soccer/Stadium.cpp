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

#include "Stadium.h"

namespace freekick
{
    namespace soccer
    {
        Stadium::Stadium (float pitchwidth, float pitchlength)
            : mPitch(new Pitch(pitchwidth, pitchlength))
        {
        }

        const boost::shared_ptr<Pitch> Stadium::getPitch() const
        {
            return mPitch;
        }
    }
}


