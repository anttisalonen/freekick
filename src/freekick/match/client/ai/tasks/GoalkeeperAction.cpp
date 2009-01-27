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

#include "tasks/GoalkeeperAction.h"

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
                    GoalkeeperAction::GoalkeeperAction (boost::shared_ptr<MatchStatus> ms, int id)
                        : mMatchStatus(ms),
                          mPlayerID(id)
                    {
                    }

                    bool GoalkeeperAction::finished() const
                    {
                        // TODO: define
                        return true;
                    }

                    boost::shared_ptr<messages::PlayerControlMessage> GoalkeeperAction::process()
                    {
                        Helpers h(mMatchStatus, mPlayerID);
                        addutil::Vector3 gotovec;

                        clearTasks();

                        if(h.own != h.b && (h.isnearestplayer || h.ballinourgoalarea))
                        {
                            // std::cerr << "GoalkeeperAction: Ball holder: " << mMatchStatus->holdingBall() << std::endl;
                            if(!h.holdingtheball)
                            {
                                // std::cerr << "GoalkeeperAction: wanting to hold the ball.\n";
                                boost::shared_ptr<GoalkeeperGetBall> t(new GoalkeeperGetBall(mMatchStatus, mPlayerID));
                                addTask(t);
                            }
                            else
                            {
                                // TODO: implement timer to wait for a while before kicking ball away
                                // std::cerr << "GoalkeeperAction: kicking the ball.\n";
                                boost::shared_ptr<ShootBall> t(new ShootBall(mMatchStatus, mPlayerID));
                                addTask(t);
                            }
                        }
                        else if (h.isnearestplayer)
                        {
                            boost::shared_ptr<ShootBall> t(new ShootBall(mMatchStatus, mPlayerID));
                            addTask(t);
                        }
                        else
                        {
                            boost::shared_ptr<IdleInFormation> t(new IdleInFormation(mMatchStatus, mPlayerID));
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
