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


#ifndef FREEKICKTASKSHELPERS_H
#define FREEKICKTASKSHELPERS_H

#include <string>

#include <boost/shared_ptr.hpp>

#include "addutil/Vector3.h"

#include "Constants.h"
#include "MatchStatus.h"

namespace freekick 
{ 
    namespace match
    {
        namespace client
        {
            class Helpers
            {
                public:
                static void correctPassVector(addutil::Vector3& target);
                static void correctShootVector(addutil::Vector3& target);
                static void correctLongBallVector(addutil::Vector3& target);
            };
        }
    }
}

#endif
