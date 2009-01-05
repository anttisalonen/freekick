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


#ifndef FREEKICKTASKSGOTOKICKOFFFORMATIONPOSITION_H
#define FREEKICKTASKSGOTOKICKOFFFORMATIONPOSITION_H

#include <boost/shared_ptr.hpp>

#include "addutil/Vector3.h"

#include "MatchStatus.h"
#include "messages/MovePlayerControlMessage.h"

#include "tasks/AtomicTask.h"

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
                    class GotoKickoffFormationPosition : public AtomicTask
                    {
                    public:
                        GotoKickoffFormationPosition(boost::shared_ptr<MatchStatus> ms, int id);
                        boost::shared_ptr<messages::PlayerControlMessage> process(bool& finished);
                    private:
                        boost::shared_ptr<MatchStatus> mMatchStatus;
                        int mPlayerID;
                        addutil::Vector3 ownformationpos;
                    };
                }
            }
        }
    }
}

#endif
