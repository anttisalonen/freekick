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

                            const BallState bss = mMatchStatus->getBallState();
                            bool allowed_to_kick = mMatchStatus->playerAllowedToKick(mPlayerID);
                            if(!allowed_to_kick)
                            {
                                if(bss.blocked_play && bss.owner == b)
                                    allowed_to_kick = true;
                            }

                            BallInOut bio = bss.bio_type;
                            bool iskickoff = (bio == Kickoff);
                            bool blocked = bss.blocked_play;
                            bool own = bss.owner;
                            bool startplay = (iskickoff && !blocked && nearestown.get<0>() == mPlayerID && own == b);
                            if(iskickoff && !startplay)       // goto start formation
                            {
                                boost::shared_ptr<GotoKickoffFormationPosition> t(new GotoKickoffFormationPosition(mMatchStatus, mPlayerID));
                                addTask(t);
                            }
                            else
                            {
                                if(nearestown.get<0>() == mPlayerID)
                                {
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
                        }

                        boost::shared_ptr<Task> nexttask = getNextTask();
/*
  if(nexttask->finished())
  {
  deleteNextTask();
  return process();
  }
*/
                        boost::shared_ptr<messages::PlayerControlMessage> msg = nexttask->process();
                        return msg;
                    }

                    boost::shared_ptr<IdleInFormation> PlaySoccer::newIdleInFormation() const
                    {
                        soccer::PlayerTarget b = mMatchStatus->getPlayerTarget(mPlayerID);
                        const boost::shared_ptr<Formation> f = mMatchStatus->getPlayerClub(mPlayerID)->getFormation();
                        PlayerPosition pp = mPlayer->getPlayerPosition();
                        addutil::Vector3 tgt = f->getPlayerArea(mPlayerID).getCenter();
                        float plength = mMatchStatus->getPitchLength();
                        float pwidth = mMatchStatus->getPitchWidth();
                        float bheight = mMatchStatus->getBall()->getPosition().z / plength;
                        float bwidth = mMatchStatus->getBall()->getPosition().x / pwidth;
                        if(b == UpTarget)
                        {
                            bheight = 1.0f - bheight;
                        }
                        // 0.0f: own goal; 1.0f: opponent's goal

                        float tgt_z_modifier = (1.0f - tgt.z) * (bheight - tgt.z) * (pp == Goalkeeper ? 0.04f : 0.4f);
                        tgt.z += tgt_z_modifier;

                        float tgt_x_modifier = (bwidth - 0.5f) * 0.2f;
                        tgt.x += tgt_x_modifier;
                        addutil::general::clamp(tgt.x, 0.0f, 1.0f);

                        if(b == UpTarget)
                        {
                            tgt.z = 1.0f - tgt.z;
                            tgt.x = 1.0f - tgt.x;
                        }

                        tgt.x *= pwidth;
                        tgt.z *= plength;
                        return (boost::shared_ptr<IdleInFormation>(new IdleInFormation(mMatchStatus, mPlayerID, tgt)));
                    }
                }
            }
        }
    }
}
