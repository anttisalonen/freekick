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


#ifndef PARSING_H
#define PARSING_H

#include <string>
#include <iostream>
#include <cstdlib>

#include <boost/lexical_cast.hpp>
#include <boost/algorithm/string/find.hpp>
#include <boost/algorithm/string/split.hpp>
#include <boost/algorithm/string/find_iterator.hpp>
#include <boost/algorithm/string/classification.hpp>

namespace addutil
{
/*
    template <typename ContT> void splitstr_and_fill(ContT& out, std::string& in, const std::string& split);
    void cut_from_string(const std::string& in, const std::string& start_token, const std::string& end_token, std::string& out);
    template <typename Cont1T, typename Cont2T, typename StringT>
        void get_int(Cont1T& nums, Cont2T& strings, const StringT& fin);
*/

    template <typename ContT> void splitstr_and_fill(ContT& out, std::string& in, const std::string& split)
    {
        using namespace std;
        using namespace boost;
        using namespace boost::algorithm;
        typedef split_iterator<string::iterator> string_split_iterator;

        for(split_iterator<string::iterator> It = make_split_iterator(in, first_finder(split, is_equal()));
            It!=string_split_iterator();
            ++It)
        {
            out.push_back(copy_range<std::string>(*It));
        }
    }

    template <typename Cont1T, typename Cont2T, typename StringT>
        void get_int(Cont1T& nums, Cont2T& strings, const StringT& fin)
    {
        using namespace std;
        bool finished = false;
        while(!finished && strings.size() > 0)
        {
            finished = (strings[0].find(fin) != string::npos);
            string& this_string = strings[0];
            string::iterator it = this_string.begin();
            string num("");
            while(*it >= '0' && *it <= '9') num += *it++;
            nums.push_back(boost::lexical_cast<int>(num));
            strings.pop_front();
        }
    }
}

#endif // PARSING_H
