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
  This file was generated on Sa Nov 22 2008 at 11:40:55
**************************************************************************/

#include "Rules.h"

// Constructors/Destructors
//  

namespace freekick
{
    namespace match
    {
        namespace server
        {
            Rules::Rules (boost::shared_ptr<MatchStatus> ms, boost::shared_ptr<Physics> p)
                : mMatchStatus(ms),
                  mPhysics(p),
                  pitch_width(mMatchStatus->getMatchData()->getStadium()->getPitch().getWidth()),
                  pitch_length(mMatchStatus->getMatchData()->getStadium()->getPitch().getLength())
            {
                mPhysics->subscribe(*this);
            }

            Rules::~Rules ( ) 
            {
                mPhysics->unsubscribe(*this);
            }

            void Rules::update(Physics* p)
            {
                PhysicsMessageList l;
                CollisionList c;
                p->getUpdates(l, c);

                bool new_ball_status = false;

                PhysicsMessageList::iterator it;
                for(it = l.begin(); it != l.end(); it++)
                {
                    int plid = it->getPlayerID();

                    // TODO: check for fouls (using collisionlist)
                    if(plid != BallID)
                        continue;

                    int der = it->getDerivative();
                    if(der != 0)
                        continue;

                    // Ball: location: checking for ball in/out
                    if(mBallState.bio_type == BallIn)
                    {
                        addutil::Vector3 vec;
                        it->getVector(vec);
                        if(vec.x < 0.0f || vec.x > pitch_width)
                        {
                            new_ball_status = true;
                            mBallState.bio_type = Throwin;
                            mBallState.flipOwner();
                            mBallState.restart_point.x = (vec.x < 0.0f) ? 0.0f : pitch_width;
                            mBallState.restart_point.z = vec.z;
                        }
                        if(vec.z < 0.0f || vec.z > pitch_length)
                        {
                            new_ball_status = true;
                            mBallState.flipOwner();
                            if((vec.z < 0.0f && mBallState.owner == Home) || (vec.z > pitch_length && mBallState.owner == Away))
                            {
                                mBallState.bio_type = Goalkick;
                            }
                            else
                            {
                                mBallState.bio_type = Cornerkick;
                            }
                            if(mBallState.bio_type == Goalkick)
                            {
                                // TODO: get rid of constants, split to functions
                                if(vec.z < 0.0f)
                                    mBallState.restart_point.z = 5.5f;
                                else
                                    mBallState.restart_point.z = pitch_length - 5.5f;

                                if(vec.x < pitch_width / 2)
                                    mBallState.restart_point.x = pitch_width / 2 - 7.5f;
                                else
                                    mBallState.restart_point.x = pitch_width / 2 + 7.5f;
                            }
                            else
                            {
                                if(vec.z < 0.0f)
                                    mBallState.restart_point.z = 0.0f;
                                else
                                    mBallState.restart_point.z = pitch_length;

                                if(vec.x < pitch_width / 2)
                                    mBallState.restart_point.x = 0.0f;
                                else
                                    mBallState.restart_point.x = pitch_width;
                            }
                        }
                    }

                    if(mBallState.bio_type == PreKickoff)
                    {
                        new_ball_status = true;
                        mBallState.bio_type = Kickoff;
                        // TODO: check if substitutes not on pitch
                    }

                    if(mBallState.bio_type == Kickoff)
                    {
                        // TODO: check if players on their sides, ball in the middle
                    }
                }

                if(new_ball_status)
                {
                    RulesMessage rm(mBallState);
                    newmessages.push_back(rm);
                }

                if(newmessages.size() > 0)
                {
                    publish();
                    mMatchStatus->update(newmessages);
                    clearMessages();
                }
            }

            void Rules::getUpdates (RulesMessageList& pes) const
            {
                pes = newmessages;
            }

            void Rules::clearMessages()
            {
                newmessages.clear();
            }
        }
    }
}

