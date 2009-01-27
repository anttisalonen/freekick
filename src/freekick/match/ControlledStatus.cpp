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

#include "ControlledStatus.h"

namespace freekick
{
    namespace match
    {
        ControlledStatus::ControlledStatus (bool c, bool hold)
            : celebrates(c),
              holding_ball(hold)
        {
        }

        void ControlledStatus::setValue(int v)
        {
            celebrates = (v % 2 == celeb_val);
            holding_ball = (v >= holdi_val);
        }

        std::ostream& operator << (std::ostream& os, const ControlledStatus& a)
        {
            int val = (a.celebrates ? ControlledStatus::celeb_val : 0) + (a.holding_ball ? ControlledStatus::holdi_val : 0);
            os << val;
            return os;
        }
    }
}
