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


#ifndef FREEKICKTASKSSOCCER_H
#define FREEKICKTASKSSOCCER_H

#include <iostream>

#include "MatchStatus.h"

#include "tasks/CompositeTask.h"
#include "tasks/StartSoccer.h"
#include "tasks/PlaySoccer.h"
#include "tasks/EndSoccer.h"

#include "messages/PlayerControlMessage.h"

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
                    class Soccer : public CompositeTask
                    {
                    public:
                        Soccer(boost::shared_ptr<MatchStatus> ms, int id);
                        boost::shared_ptr<messages::PlayerControlMessage> process(bool& finished);
                    private:
                        boost::shared_ptr<MatchStatus> mMatchStatus;
                        int mPlayerID;
                    };
                }
            }
        }
    }
}

#endif
