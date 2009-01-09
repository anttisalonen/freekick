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
                                // TODO: read max. kicking distance from somewhere else
                                if(nearestown.get<1>() < 1.5f)    // able to kick
                                {
                                    std::cerr << "Kicking\n";
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
                                const boost::shared_ptr<Formation> f = mMatchStatus->getPlayerClub(mPlayerID)->getFormation();
                                addutil::Vector3 tgt = f->getPlayerArea(mPlayerID).getCenter();
                                if(b == Away)
                                {
                                    tgt.z = 1.0f - tgt.z;
                                }
                                tgt.x *= mMatchStatus->getPitchWidth();
                                tgt.z *= mMatchStatus->getPitchLength();
                                boost::shared_ptr<IdleInFormation> t(new IdleInFormation(mMatchStatus, mPlayerID, tgt));
                                addTask(t);
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
                }
            }
        }
    }
}
