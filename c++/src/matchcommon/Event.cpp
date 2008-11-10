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

void istream_to_string(std::istream& str, std::string& s)
{
    bool sws = str.flags() | std::ios::skipws;
    str.unsetf(std::ios_base::skipws);
    str >> std::noskipws >> s;
    if(sws) str.setf(std::ios_base::skipws);
}

void parse_events(std::string& st, std::deque<std::string>& evts)
{
    static std::string data = "";
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
                    if (data.length() > 0)
                        evts.push_back(data);
                    data = "";
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

/*
void parse_events(std::istringstream& str, std::deque<std::string>& evts)
{
    static std::string data = "";
    static bool in = false;
    char c = 0;
    while(str.good())
    {
        if(in)
        {
            while(str.good() && c != '\n')
            {
                c = str.get();
                if (c == ')')
                {
                    in = false;
                    if (data.length() > 0)
                        evts.push_back(data);
                    data = "";
                    break;
                }
                if (isprint(c))
                    data += c;
            }
        }
        else
        {
            c = str.get();
            if (c == '(')
                in = true;
        }
    }
*/
/*
    char c = 0;
    std::string data;
    while (str.good())
    {
        c = str.get();
        data += c;
    }
    evts.push_back(data);
*/
/*
    char c;
    std::string data;
    str >> std::noskipws;
    while(str >> c)
    {
        if (c == '(') data = "";
        else if (c == ')')
        {
            evts.push_back(data);
            data = "";
        }
        else data += c;
    }
*/
/*
}
*/
