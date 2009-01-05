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

#include "tasks/PlaySoccer.h"

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
                    PlaySoccer::PlaySoccer (boost::shared_ptr<MatchStatus> ms, int id)
                        : mMatchStatus(ms),
                          mPlayerID(id)
                    {
                    }

                    boost::shared_ptr<messages::PlayerControlMessage> PlaySoccer::process(bool& finished)
                    {
                        finished = false;
                        tasks::GotoKickoffFormationPosition nexttask(mMatchStatus, mPlayerID);
                        boost::shared_ptr<messages::PlayerControlMessage> msg = nexttask.process(finished);
                        return msg;
                    }
                }
            }
        }
    }
}
