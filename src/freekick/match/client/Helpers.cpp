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

#include "Helpers.h"

namespace freekick 
{ 
    namespace match
    {
        namespace client
        {
            void Helpers::correctPassVector(addutil::Vector3& target)
            {
                if (target.length() > 20.0f)
                {
                    target *= 1.5f;
                }
                target.y = target.length() * 0.1f;
                target.x *= 1.1f;
                target.z *= 1.1f;
            }

            void Helpers::correctShootVector(addutil::Vector3& target)
            {
                target.y = target.length() * 0.2f;
                target.normalize();
                target *= 30.0f;
            }

            void Helpers::correctLongBallVector(addutil::Vector3& target)
            {
                target.y = target.length() * 0.4f;
                target.normalize();
                target *= 30.0f;
            }
        }
    }
}
