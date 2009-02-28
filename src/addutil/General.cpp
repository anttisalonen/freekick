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

    }
}
