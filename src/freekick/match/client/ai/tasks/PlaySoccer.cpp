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
                        soccer::BallOwner b = mMatchStatus->getPlayerSide(mPlayerID);
                        boost::tuple<int, float> nearestown = mMatchStatus->nearestPlayerFromClubToBall(b);

                        bool issub = mPlayer->isSubstitute();
                        if(issub)
                        {
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
                                clearTasks();
                            }

                            if(nearestown.get<0>() == mPlayerID)
                            {
                                bool allowed_to_kick = mMatchStatus->playerAllowedToKick(mPlayerID);
                                if(!allowed_to_kick)
                                {
                                    BallState bss = mMatchStatus->getBallState();
                                    if(bss.blocked_play && bss.owner == b)
                                        allowed_to_kick = true;
                                }

                                if(allowed_to_kick)
                                {
                                    // TODO: read max. kicking distance from somewhere else
                                    if(nearestown.get<1>() < 1.5f)    // able to kick
                                    {
                                        boost::shared_ptr<KickBall> t(new KickBall(mMatchStatus, mPlayerID));
                                        addTask(t);
                                    }
                                    else
                                    {
                                        boost::tuple<int, float> nearestother = mMatchStatus->nearestPlayerFromClubToBall(other(b));
                                        if(nearestown.get<1>() < nearestother.get<1>())         // run to ball
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
                                    addTask(newIdleInFormation());
                                }
                            }
                            else
                            {
                                addTask(newIdleInFormation());
                            }
                        }

                        boost::shared_ptr<Task> nexttask = getNextTask();
                        if(nexttask->finished())
                        {
                            deleteNextTask();
                            return process();
                        }
                        boost::shared_ptr<messages::PlayerControlMessage> msg = nexttask->process();
                        return msg;
                    }

                    boost::shared_ptr<IdleInFormation> PlaySoccer::newIdleInFormation() const
                    {
                        soccer::BallOwner b = mMatchStatus->getPlayerSide(mPlayerID);
                        const boost::shared_ptr<Formation> f = mMatchStatus->getPlayerClub(mPlayerID)->getFormation();
                        addutil::Vector3 tgt = f->getPlayerArea(mPlayerID).getCenter();
                        if(b == Away)
                        {
                            tgt.z = 1.0f - tgt.z;
                        }
                        tgt.x *= mMatchStatus->getPitchWidth();
                        tgt.z *= mMatchStatus->getPitchLength();
                        return (boost::shared_ptr<IdleInFormation>(new IdleInFormation(mMatchStatus, mPlayerID, tgt)));
                    }
                }
            }
        }
    }
}
