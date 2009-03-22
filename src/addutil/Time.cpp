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

#include "Time.h"

namespace addutil
{
    Time::Time (int hh, int mm, int ss, float mss)
        : h(hh), m(mm), s(ss), ms(mss) 
    {
    }

    void Time::add_m(int _m)
    {
        m += _m;
    }

    void Time::add_s(float _s)
    {
        add_ms(s * 1000.0f);
    }

    void Time::add_s(int _s)
    {
        int final_s = (s + _s) % 60;
        int additional_m = (s + _s) / 60;
        s = final_s;
        if(additional_m)
            add_m(additional_m);
    }

    void Time::add_ms(float _ms)
    {
        float final_ms = std::fmod((_ms + ms), 1000);
        int additional_s = (_ms + ms) / 1000;
        ms = final_ms;
        if(additional_s > 0)
            add_s(additional_s);
    }

    void Time::reset()
    {
        h = m = s = 0;
        ms = 0.0f;
    }
}

