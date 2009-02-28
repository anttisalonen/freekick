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
                        mPlayer = mMatchStatus->getPlayer(mPlayerID);
                    }

                    bool PlaySoccer::finished() const
                    {
                        const BallState bs = mMatchStatus->getBallState();
                        if(bs.bio_type == HalfFullTime || bs.bio_type == PreKickoff)
                        {
                            return true;
                        }
                        return false;
                    }

                    boost::shared_ptr<messages::PlayerControlMessage> PlaySoccer::process()
                    {
                        Helpers h(mMatchStatus, mPlayerID);

/*
  boost::shared_ptr<Task> nexttask = getNextTask();
  if(nexttask->finished())
  {
  deleteNextTask();
  }
*/
                        clearTasks();

                        if(h.issub)
                        {
                            // TODO: replace with BeingASubstituteTask
                            if(emptyTasks())
                            {
                                boost::shared_ptr<Idle> t(new Idle(mPlayerID));
                                addTask(t);
                            }
                        }
                        else
                        {
                            if(!emptyTasks())
                            {
                                // TODO: check if current task should be cleared
                                clearTasks();
                            }

                            if(h.bio == BallIn && mPlayer->getPlayerPosition() == Goalkeeper)
                            {
                                boost::shared_ptr<GoalkeeperAction> t(new GoalkeeperAction(mMatchStatus, mPlayerID));
                                addTask(t);
                            }
                            else
                            {
                                if(h.iskickoff && !h.startplay)       // goto start formation
                                {
                                    boost::shared_ptr<GotoKickoffFormationPosition> t(new GotoKickoffFormationPosition(mMatchStatus, mPlayerID));
                                    addTask(t);
                                }
                                else
                                {
                                    addutil::Vector3 ballpos_corrected = 
                                        mMatchStatus->absolute_pitch_position_to_percent(mMatchStatus->getBall()->getPosition(), h.b);
                                    bool ballinmyarea = mMatchStatus->getPlayerFormation(mPlayerID)->getPlayerArea(mPlayerID).in(
                                        ballpos_corrected);
                                    if((h.isnearestplayer || ballinmyarea) && h.allowed_to_kick)
                                    {
                                        if(h.abletokick)
                                        {
                                            boost::shared_ptr<KickBall> t(new KickBall(mMatchStatus, mPlayerID));
                                            addTask(t);
                                        }
                                        else
                                        {
                                            if(h.amnearerthanthey)         // run to ball
                                            {
                                                boost::shared_ptr<FetchBall> t(new FetchBall(mMatchStatus, mPlayerID));
                                                addTask(t);
                                            }
                                            else                                                    // defensive
                                            {
                                                boost::shared_ptr<FetchBall> t(new FetchBall(mMatchStatus, mPlayerID));
                                                addTask(t);
                                            }
                                        }
                                    }
                                    else
                                    {
                                        boost::shared_ptr<IdleInFormation> t(new IdleInFormation(mMatchStatus, mPlayerID));
                                        addTask(t);
                                    }
                                }
                            }
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
