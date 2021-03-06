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


#ifndef STADIUM_H
#define STADIUM_H

#include <string>

#include <boost/serialization/serialization.hpp>
#include <boost/shared_ptr.hpp>

#include "addutil/Color.h"

#include "Pitch.h"

/**
 * class Stadium
 */

namespace freekick
{
    namespace soccer
    {
        class Stadium
        {
        public:
            Stadium (float pitchwidth = 70.0f, float pitchlength = 100.0f);
            const boost::shared_ptr<Pitch> getPitch() const;

        private:
            boost::shared_ptr<Pitch> mPitch;

            friend class boost::serialization::access;
            template<class Archive>
                void serialize(Archive & ar, const unsigned int version)
            {
                ar & mPitch;
            }
        };
    }
}

#endif // STADIUM_H
