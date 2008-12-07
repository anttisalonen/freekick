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
**************************************************************************/

#include "messages/Message.h"

namespace freekick
{
    namespace match
    {
        namespace messages
        {
            std::string setToMessageList(const std::set<PlayerID>& c)
            {
                std::ostringstream oss(std::ostringstream::out);
                oss << list_start;
                std::set<PlayerID>::iterator it;
                it = c.begin();

                if(it != c.end())
                {
                    while(1)
                    {
                        oss << *it;
                        it++;
                        if(it != c.end())
                        {
                            oss << list_delim;
                        }
                        else
                        {
                            break;
                        }
                    }
                }
                oss << list_end;
                return oss.str();
            }
        }
    }
}
