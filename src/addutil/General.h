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


#ifndef ADDUTIL_GENERAL_H
#define ADDUTIL_GENERAL_H

#include <set>

#include <boost/foreach.hpp>

namespace addutil
{
    namespace general
    {
        template <typename T> void clamp(T& val, T min, T max)
        {
            if(val < min) val = min;
            if(val > max) val = max;
        }

        template <typename T> void set_union(const std::set<T>& a, const std::set<T>& b, std::set<T>& out)
        {
            out.clear();
            BOOST_FOREACH(T n, a)
            {
                if(b.find(n) != b.end())
                {
                    out.insert(n);
                }
            }
        }

        template <typename T> void set_inverse_union(const std::set<T>& more, const std::set<T>& less, std::set<T>& diff)
        {
            diff.clear();
            BOOST_FOREACH(T n, more)
            {
                if(less.find(n) == less.end())
                {
                    diff.insert(n);
                }
            }
        }
    }
}

#endif
