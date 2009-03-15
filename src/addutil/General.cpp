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

#include "General.h"

namespace addutil
{
    namespace general
    {
        float rand_float()
        {
            return (rand() / (float)RAND_MAX);
        }

        float rand_float(float min, float max)
        {
            return rand_float() * (max - min) + min;
        }

        int rand_int(int max)
        {
            return (rand() % max);
        }

        float rand_std_normal_distribution()
        {
            float x1, x2, q;
            do
            {
                x1 = rand_float();
                x2 = rand_float();
                q = (2 * x1 - 1.0f) * (2 * x1 - 1.0f) + (2 * x2 - 1.0f) * (2 * x2 - 1.0f);
            } while (q > 1.0f);
            float p = sqrt((-2.0f * log(q)) / q);
            float z1 = (2 * x1 - 1) * p;
            // float z2 = (2 * x2 - 1) * p;
            return z1;
        }

        int rand_normal_distribution(int min, int max)
        {
            int half_range = (max - min) / 2;
            int mid = half_range + min;
            if(half_range < 5) return min;
            int val;
            do
            {
                float f = rand_std_normal_distribution();
                val = f * 0.3f * half_range;
                val += mid;
            } while (val > max || val < min);
            return val;
        }

        void colorbyte_to_color(char color, int& r, int& g, int& b)
        {
            switch(color)
            {
                case 0:   // grey
                    r = g = b = 128; break;
                case 1:   // white
                    r = g = b = 255; break;
                case 2:   // black
                    r = g = b = 0; break;
                case 3:   // orange
                    r = 255;
                    g = 127;
                    b = 0;
                    break;
                case 4:   // red
                    r = 255;
                    g = b = 0;
                    break;
                case 5:   // blue
                    b = 255;
                    r = g = 0;
                    break;
                case 6:   // brown
                    r = 150;
                    g = 75;
                    b = 0;
                    break;
                case 7:   // light blue
                    r = 173;
                    g = 216;
                    b = 230;
                    break;
                case 8:   // green
                    r = b = 0;
                    g = 255;
                    break;
                case 9:   // yellow
                    r = g = 255;
                    b = 0;
                    break;
                default:
                    r = g = b = 255;
                    break;
            }
        }

        void name_to_first_and_last_name(const char* name, std::string& first_name, std::string& last_name)
        {
            first_name = "";
            last_name = "";
            const char* iter = name;
            bool last = false;
            while(*iter != 0)
            {
                if(*iter == ' ' && last == false)
                {
                    last = true;
                }
                else
                {
                    if(!last)
                    {
                        first_name.append(1, *iter);
                    }
                    else
                    {
                        last_name.append(1, *iter);
                    }
                }
                iter++;
            }
        }

    }
}
