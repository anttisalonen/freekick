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

#ifndef AIENGINE_H
#define AIENGINE_H

#include "MatchStatus.h"
#include "Network.h"

namespace freekick
{
    namespace match
    {
        namespace client
        {
            namespace ai_client
            {
                class AI_Engine
                {
                public:
                    AI_Engine(MatchStatus* st, Network* netw);
                    virtual ~AI_Engine() { }
                    void run();

                private:
                    MatchStatus* mMatchStatus;
                    Network* mNetwork;
                };
            }
        }
    }
}

#endif
