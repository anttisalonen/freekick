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

#include "tasks/Soccer.h"

namespace freekick 
{ 
    namespace match
    {
        namespace client
        {
            namespace ai_client
            {
                namespace tasks
                {
                    Soccer::Soccer (boost::shared_ptr<MatchStatus> ms, int id)
                        : mMatchStatus(ms),
                          mPlayerID(id)
                    {
                        boost::shared_ptr<tasks::StartSoccer> t1;
                        boost::shared_ptr<tasks::PlaySoccer> t2;
                        boost::shared_ptr<tasks::EndSoccer> t3;
                        int num_half_times = 2;
                        for(int i = 0; i < num_half_times; i++)
                        {
                            t1.reset(new tasks::StartSoccer(mMatchStatus, mPlayerID));   // 1, 4
                            addTask(t1);
                            t2.reset(new tasks::PlaySoccer(mMatchStatus, mPlayerID));    // 2, 5
                            addTask(t2);
                            t3.reset(new tasks::EndSoccer(mMatchStatus, mPlayerID));     // 3, 6
                            addTask(t3);
                        }
                    }
                }
            }
        }
    }
}
