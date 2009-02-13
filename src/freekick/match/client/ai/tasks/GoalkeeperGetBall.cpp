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

#include "tasks/GoalkeeperGetBall.h"

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
                    GoalkeeperGetBall::GoalkeeperGetBall (boost::shared_ptr<MatchStatus> ms, int id)
                        : mMatchStatus(ms),
                          mPlayerID(id)
                    {
                        mPlayer = mMatchStatus->getPlayer(mPlayerID);
                    }

                    bool GoalkeeperGetBall::finished() const
                    {
                        return true;
                    }

                    boost::shared_ptr<messages::PlayerControlMessage> GoalkeeperGetBall::process()
                    {
                        addutil::Vector3 ownpos = mPlayer->getPosition();
                        addutil::Vector3 ballpos = mMatchStatus->getBall()->getPosition();
                        addutil::Vector3 ballvel = mMatchStatus->getBall()->getVelocity();
                        float balldist = (ownpos - ballpos).length();

                        ballpos.y = 0.0f;
                        ballvel.y = 0.0f;
                        ownpos.y = 0.0f;

                        float maxvelocity = 10.0f;         // of the player
                        addutil::Vector3 gotovec = steering::pursuit(ownpos, ballpos, ballvel, maxvelocity);

                        using namespace messages;
                        if(balldist < 2.5f)      // TODO: get length from somewhere else
                        {
                            return boost::shared_ptr<HoldPlayerControlMessage>(new HoldPlayerControlMessage(mPlayerID, gotovec));
                        }
                        else
                        {
                            return boost::shared_ptr<MovePlayerControlMessage>(new MovePlayerControlMessage(mPlayerID, gotovec));
                        }
                    }
                }
            }
        }
    }
}