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

#include "tasks/KickBall.h"

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
                    KickBall::KickBall (const boost::shared_ptr<MatchStatus>& ms, int id)
                        : mMatchStatus(ms),
                          mPlayerID(id)
                    {
                        mPlayer = mMatchStatus->getPlayer(mPlayerID);
                    }

                    bool KickBall::finished() const
                    {
                        // return true;
                        // float len = (mPlayer->getPosition() - mTarget).length();
                        // if(len < 5.0f) return true;
                        return false;
                    }

                    boost::shared_ptr<messages::PlayerControlMessage> KickBall::process()
                    {
                        clearTasks();

                        // TODO: have shootball, passball share these
                        addutil::Vector3 ownpos = mPlayer->getPosition();
                        soccer::PlayerTarget t = mMatchStatus->getPlayerTarget(mPlayerID);
                        addutil::Vector3 tgtgoal = mMatchStatus->getGoalPosition(t);
                        addutil::Vector3 goalvec = tgtgoal - ownpos;

                        if(goalvec.length() < 35.0f)
                        {
                            boost::shared_ptr<ShootBall> t(new ShootBall(mMatchStatus, mPlayerID));
                            addTask(t);
                        }
                        else
                        {
                            boost::shared_ptr<PassBall> t(new PassBall(mMatchStatus, mPlayerID));
                            addTask(t);
                        }


                        boost::shared_ptr<Task> nexttask = getNextTask();
                        boost::shared_ptr<messages::PlayerControlMessage> msg = nexttask->process();
                        return msg;
                    }
                }
            }
        }
    }
}
