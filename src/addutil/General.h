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

#include <cmath>
#include <set>
#include <iostream>
#include <string>

#include <boost/foreach.hpp>

namespace addutil
{
    namespace general
    {
/**
 * clamp: sets val between min and max
 */
        template <typename T> void clamp(T& val, T min, T max)
        {
            if(val < min) val = min;
            if(val > max) val = max;
        }

/**
 * intervalize: sets val to be divisible by interval.
 */
        template <typename T> void intervalize(T& val, T interval)
        {
            T dif = fmod(val, interval);
            if(dif < interval / 2.0f)
            {
                val -= dif;
            }
            else
                val += (interval - dif);
        }

/**
 * set_union: sets out to the union of a and b.
 * e.g.: a:   1   3 4
 *       b:     2 3   5
 *       out:     3
 */
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

/**
 * set_inverse_union: sets diff to what more has that less doesn't.
 * e.g.: more: 1 2 3 4
 *       less:   2 3 4
 *       diff: 1
 */
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

        /**
         * "normalizes" a value, i.e. returns a point between 0 and 1.
         * e.g.: say you have a value 40, and you want to normalize it
         * to be 0.4 as 100 is the maximum. maxval = 100, point = 40.
         * return value: 0.4.
         */
        template <typename T> T normalize(const T& maxval, const T& point)
        {
            return std::max(0.0f, maxval - point / maxval);
        }

        float rand_float();
        float rand_float(float min, float max);
        int rand_int(int max);
        float rand_std_normal_distribution();
        int rand_normal_distribution(int min, int max);
        void colorbyte_to_color(char color, int& r, int& g, int& b);
        void name_to_first_and_last_name(const char* name, std::string& first_name, std::string& last_name);
    }
}

#endif
