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


#ifndef FREEKICKTASKSGOTOCABINS_H
#define FREEKICKTASKSGOTOCABINS_H

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
                    class GotoCabins : public AtomicTask
                    {
                    public:
                        GotoCabins(boost::shared_ptr<MatchStatus> ms, int id);
                        bool finished() const;
                        boost::shared_ptr<messages::PlayerControlMessage> process();
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
