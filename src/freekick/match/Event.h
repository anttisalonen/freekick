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

#include <vector>
#include <deque>
#include <string>
#include <iostream>
#include <cctype>

namespace freekick
{
    namespace match
    {
        //! turns "(evt1)(evt2)" into "(evt1)", "(evt2)"
        void parse_events(std::string& st, std::deque<std::string>& evts, bool keep_parens = false);
    }
}
