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

#include "AIConfig.h"

namespace freekick
{
    namespace match
    {
        namespace client
        {
            namespace ai_client
            {
                AIConfig* AIConfig::instance = 0;
                AIConfig* AIConfig::getInstance()
                {
                    if(instance == 0)
                        instance = new AIConfig();
                    return instance;
                }

                AIConfig::AIConfig()
                {
                    verbose = 0;
                    max_future_fetch_distance = 3.0f;
                    future_lookup_time = 1.0f;
                }
            }
        }
    }
}
