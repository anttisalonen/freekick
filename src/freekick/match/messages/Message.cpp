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

            bool messageListToSet(std::string m, std::set<PlayerID>& s)
            {
                std::string::iterator it;
                int state = 0;
                PlayerID n;
                std::string num("");
                std::stringstream nums;
                s.clear();
                for (it = m.begin(); it < m.end(); it++)
                {
                    if(isspace(*it)) continue;
                    switch(state)
                    {
                        case -1:
                            return false;
                        case 0:
                            std::cout << "State 0: " << *it << std::endl;
                            if (*it == '[')
                            {
                                state = 1;
                                break;
                            }
                            return false;
                        case 1:
                            std::cout << "State 1: " << *it << std::endl;
                            num = "";
                            while(isdigit(*it))
                            {
                                num += *it;
                                it++;
                            }
                            it--;
                            nums.str(num);
                            nums >> n;
                            std::cout << "n: " << n << std::endl;
                            if (n == 0)
                                return false;
                            s.insert(n);
                            state = 2;
                            break;
                        case 2:
                            std::cout << "State 2: " << *it << std::endl;
                            if(*it == ']')
                            {
                                state = 3;
                                break;
                            }
                            else if (*it == ',')
                            {
                                state = 1;
                                break;
                            }
                            return false;
                        case 3:
                            std::cout << "State 3: " << *it << std::endl;
                        default:
                            return false;
                    }
                }
                return true;
            }
        }
    }
}
