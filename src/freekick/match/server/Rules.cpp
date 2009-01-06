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
                  mPitch(mMatchStatus->getMatchData()->getStadium()->getPitch())
            {
                mPhysics->subscribe(*this);

                players[0]     = mMatchStatus->getMatchData()->getHomeClub()->getLineup().getPitchPlayerIDs();
                substitutes[0] = mMatchStatus->getMatchData()->getHomeClub()->getLineup().getSubstituteIDs();
                players[1]     = mMatchStatus->getMatchData()->getAwayClub()->getLineup().getPitchPlayerIDs();
                substitutes[1] = mMatchStatus->getMatchData()->getAwayClub()->getLineup().getSubstituteIDs();

                last_update_time = boost::posix_time::microsec_clock::local_time();
            }

            Rules::~Rules ( ) 
            {
                mPhysics->unsubscribe(*this);
            }

            void Rules::update(Physics* p)
            {
                PhysicsMessageList l;
                OwnerMessageList c;
                p->getUpdates(l, c);

                bool new_ball_status = false;
                bool ball_touched = false;

                OwnerMessageList::iterator oit;
                for(oit = c.begin(); oit != c.end(); oit++)
                {
                    try
                    {
                        BallOwner b = mMatchStatus->getPlayerClub(oit->getOwnerID());
                        if(mBallState.bio_type == BallIn)
                        {
                            if(b != mBallState.owner)
                            {
                                mBallState.owner = b;
                                new_ball_status = true;
                            }
                        }
                        ball_touched = true;
                    }
                    catch(...)
                    {
                        // invalid ID in owner message (referee, etc.) -> ignore.
                    }
                }

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

                    float pitch_width = mPitch->getWidth();
                    float pitch_length = mPitch->getLength();

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
                            mBallState.blocked_play = true;
                        }
                        if(vec.z < 0.0f || vec.z > pitch_length)
                        {
                            new_ball_status = true;
                            mBallState.flipOwner();
                            mBallState.blocked_play = true;
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

                    if(mBallState.bio_type == PreKickoff || mBallState.bio_type == Kickoff)
                    {
                        typedef std::map<int, boost::shared_ptr<MatchPlayer> > EntityMap;
                        std::map<int, boost::shared_ptr<MatchPlayer> > entitymap;
                        mMatchStatus->getPlayers(entitymap);
                        std::vector<int>::const_iterator it;

                        if(mBallState.bio_type == PreKickoff)
                        {
                            bool substitute_on_pitch = false;

                            for(int i = 0; i < 2 && substitute_on_pitch == false; i++)
                            {
                                for(it = substitutes[i].begin(); it != substitutes[i].end() && substitute_on_pitch == false; it++)
                                {
                                    EntityMap::const_iterator eit = entitymap.find(*it);
                                    if(eit != entitymap.end())
                                    {
                                        addutil::Vector3 v = eit->second->getPosition();
                                        if(mPitch->onPitch(v.x, v.z))
                                            substitute_on_pitch = true;
                                    }
                                }
                            }
                            if(!substitute_on_pitch)
                            {
                                new_ball_status = true;
                                mBallState.bio_type = Kickoff;
                                mBallState.blocked_play = true;
                            }
                        }
                        else if(mBallState.bio_type == Kickoff)
                        {
                            // TODO: check if a player moved from his side to the other after giving play free
                            if(mBallState.blocked_play)
                            {
                                bool player_not_on_his_side = false;
                                for(int i = 0; i < 2 && player_not_on_his_side == false; i++)
                                {
                                    for(it = players[i].begin(); it != players[i].end() && player_not_on_his_side == false; it++)
                                    {
                                        EntityMap::const_iterator eit = entitymap.find(*it);
                                        if(eit != entitymap.end())
                                        {
                                            addutil::Vector3 v = eit->second->getPosition();
                                            // TODO: check for half time/coin toss
                                            if(!mPitch->onSide((i == 0), v.x, v.z))
                                                player_not_on_his_side = true;
                                        }
                                    }
                                }
                                if(!player_not_on_his_side)
                                {
                                    new_ball_status = true;
                                    mBallState.blocked_play = false;
                                }
                                // TODO: check for ball in centre
                            }
                            else
                            {
                                if(ball_touched)
                                {
                                    new_ball_status = true;
                                    mBallState.bio_type = BallIn;
                                }
                            }
                        }
                    }
                }

                boost::posix_time::ptime this_time(boost::posix_time::microsec_clock::local_time());
                boost::posix_time::time_period diff_time(last_update_time, this_time);
                boost::posix_time::time_duration diff_dur = diff_time.length();
                unsigned long us_diff = diff_dur.total_microseconds();
                if(us_diff > update_time_interval_ms) new_ball_status = true;

                if(new_ball_status)
                {
                    RulesMessage rm(mBallState);
                    newmessages.push_back(rm);
                }

                if(newmessages.size() > 0)
                {
                    last_update_time = this_time;
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

