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
  This file was generated on So Okt 26 2008 at 12:09:20
**************************************************************************/


#ifndef TIME_H
#define TIME_H

#include <cmath>
#include <iostream>

#include <boost/serialization/serialization.hpp>

namespace addutil
{
    class Time
    {
    public:
        Time (int hh = 0, int mm = 0, int ss = 0, float mss = 0 );

        int h;
        int m;
        int s;
        float ms;
        void add_m(int _m);
        void add_s(float s);
        void add_s(int _s);
        void add_ms(float _ms);
        void reset();

    private:

        friend class boost::serialization::access;
        template<class Archive>
            void serialize(Archive & ar, const unsigned int version)
        {
            ar & h;
            ar & m;
            ar & s;
            ar & ms;
        }
    };
}

#endif // TIME_H
