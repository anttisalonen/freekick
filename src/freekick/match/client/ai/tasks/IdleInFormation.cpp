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

#include "tasks/IdleInFormation.h"

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
                    IdleInFormation::IdleInFormation (const boost::shared_ptr<MatchStatus>& ms, int id, const addutil::Vector3& form)
                        : mMatchStatus(ms),
                          mPlayerID(id),
                          mTarget(form)
                    {
                        mPlayer = mMatchStatus->getPlayer(mPlayerID);
                    }

                    bool IdleInFormation::finished() const
                    {
                        // return true;
                        // float len = (mPlayer->getPosition() - mTarget).length();
                        // if(len < 5.0f) return true;
                        return false;
                    }

                    boost::shared_ptr<messages::PlayerControlMessage> IdleInFormation::process()
                    {
                        addutil::Vector3 gotovec = mTarget - mPlayer->getPosition();
                        if(gotovec.length() < 1.0f) gotovec.reset();
                        gotovec.y = 0.0f;

                        using namespace messages;
                        return boost::shared_ptr<MovePlayerControlMessage>(new MovePlayerControlMessage(mPlayerID, gotovec));
                    }
                }
            }
        }
    }
}
