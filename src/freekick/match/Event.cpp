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

#include "Event.h"

namespace freekick
{
    namespace match
    {
        void istream_to_string(std::istream& str, std::string& s)
        {
            bool sws = str.flags() | std::ios::skipws;
            str.unsetf(std::ios_base::skipws);
            str >> std::noskipws >> s;
            if(sws) str.setf(std::ios_base::skipws);
        }

        void parse_events(std::string& st, std::deque<std::string>& evts, bool keep_parens)
        {
            static std::string data = "(";
            static bool in = false;
            char c = 0;
            std::string::iterator it = st.begin();
            while(it != st.end())
            {
                if(in)
                {
                    while(it != st.end() && c != '\n')
                    {
                        c = *it;
                        it++;
                        if (c == ')')
                        {
                            in = false;
                            if ((data.length() > 0 && !keep_parens) || (data.length() > 1 && keep_parens))
                            {
                                if(keep_parens) data += ")";
                                evts.push_back(data);
                            }
                            if (keep_parens) data = "(";
                            else data = "";
                            break;
                        }
                        if (isprint(c))
                            data += c;
                    }
                }
                else
                {
                    c = *it;
                    it++;
                    if (c == '(')
                        in = true;
                }
            }
        }
    }
}
