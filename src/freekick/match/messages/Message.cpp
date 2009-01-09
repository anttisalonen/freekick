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

            bool isValidMessage(const std::string& s)
            {
                int ssiz = s.size();
                if(ssiz < 2) return false;
                if(s[0] != '(') return false;
                // if(s[ssiz - 1] != ')') return false;
                return true;
            }

            const std::string getMessageType(const std::string& s)
            {
                if(!isValidMessage(s)) throw "getMessageType: Invalid event\n";
                const char& c = s[1];
                if(isdigit(c) || c == '-') return s_const_upd;
                return std::string(1, c);
            }

            int getSerializationMessageType(const std::string& s)
            {
                if(getMessageType(s) != serialization_delim) throw "getSerializationMessageType: not a serialization message\n";
                std::string n(s, 3, 3);
                int i = atoi(n.c_str());
                return i;
            }

            bool messageListToSet(const std::string& m, std::set<PlayerID>& s)
            {
                std::string::const_iterator it;
                int state = 0;
                PlayerID n;
                std::string num("");
                s.clear();
                for (it = m.begin(); it < m.end(); it++)
                {
                    if(isspace(*it)) continue;
                    if(*it == ']' && state == 1)
                    {
                        state = 3; 
                        continue;
                    }
                    switch(state)
                    {
                        case -1:
                            return false;
                        case 0:
                            if (*it == '[')
                            {
                                state = 1;
                                break;
                            }
                            return false;
                        case 1:
                            num = "";
                            while(isdigit(*it))
                            {
                                num += *it;
                                it++;
                            }
                            it--;
                            n = atoi(num.c_str());
                            std::cout << "n: " << n << std::endl;
                            if (n == 0)
                                return false;
                            s.insert(n);
                            state = 2;
                            break;
                        case 2:
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
                        default:
                            return false;
                    }
                }
                if(state == 3)
                    return true;
                return false;
            }
        }
    }
}
