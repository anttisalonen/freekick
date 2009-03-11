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

                players[0]     = mMatchStatus->getMatchData()->getHomeClub()->getLineup()->getPitchPlayerIDs();
                substitutes[0] = mMatchStatus->getMatchData()->getHomeClub()->getLineup()->getSubstituteIDs();
                players[1]     = mMatchStatus->getMatchData()->getAwayClub()->getLineup()->getPitchPlayerIDs();
                substitutes[1] = mMatchStatus->getMatchData()->getAwayClub()->getLineup()->getSubstituteIDs();

                last_update_time = boost::posix_time::microsec_clock::local_time();
            }

            Rules::~Rules ( ) 
            {
                mPhysics->unsubscribe(*this);
            }

            void Rules::check_for_touched_ball(const OwnerMessageList& c, bool& new_ball_status, bool& ball_touched)
            {
                OwnerMessageList::const_iterator oit;
                for(oit = c.begin(); oit != c.end(); oit++)
                {
                    try
                    {
                        soccer::BallOwner b = mMatchStatus->getPlayerSide(oit->getOwnerID());
                        if(mBallState.bio_type == BallIn)
                        {
                            if(b != mBallState.owner)
                            {
                                mBallState.owner = b;
                                new_ball_status = true;
                            }
                        }
                        else
                        {
                            if(mBallState.owner == b && !mBallState.blocked_play)
                            {
                                ball_touched = true;
                            }
                            else
                            {
                                std::cerr << "Rules::update: Player kicks the ball but was not allowed (play blocked/ball out of play)\n";
                            }
                        }
                    }
                    catch(...)
                    {
                        // invalid ID in owner message (referee, etc.) -> ignore.
                    }
                }
            }

            bool Rules::ball_due_for_throwin(const addutil::Vector3 ballvec) const
            {
                if(ballvec.z > 0.0f && ballvec.z < mPitch->getLength() && 
                   (ballvec.x < 0.0f || ballvec.x > mPitch->getWidth()))
                {
                    return true;
                }
                return false;
            }

            bool Rules::ball_over_a_goal_line(const addutil::Vector3& ballvec) const
            {
                return (ballvec.z < 0.0f || ballvec.z > mPitch->getLength());
            }

            void Rules::check_for_over_pitch(const PhysicsMessageList& l, bool& new_ball_status, GoalQuery& goal_scored)
            {
                PhysicsMessageList::const_iterator it;
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
                        if(ball_due_for_throwin(vec))
                        {
                            new_ball_status = true;
                            mBallState.bio_type = Throwin;
                            mBallState.flipOwner();
                            mBallState.restart_point.x = (vec.x < 0.0f) ? 0.0f : pitch_width;
                            mBallState.restart_point.z = vec.z;
                            mBallState.blocked_play = true;
                        }
                        else if(ball_over_a_goal_line(vec))
                        {
                            new_ball_status = true;
                            mBallState.flipOwner();
                            mBallState.blocked_play = true;
                            bool fst_goal = mPitch->inFirstGoal(vec);
                            bool snd_goal = mPitch->inSecondGoal(vec);

                            if(fst_goal || snd_goal)
                            {
                                mBallState.bio_type = Kickoff;
                                mBallState.restart_point.x = pitch_width / 2.0f;
                                mBallState.restart_point.z = pitch_length / 2.0f;
                            }

                            if(fst_goal)                // goal (1)
                            {
                                mMatchStatus->addAwayScore();
                                goal_scored = AwayGoal;
                                mBallState.owner = Home;
                            }
                            else if(snd_goal)           // goal (2)
                            {
                                mMatchStatus->addHomeScore();
                                goal_scored = HomeGoal;
                                mBallState.owner = Away;
                            }
                            else if((vec.z < 0.0f && mBallState.owner == Home) || (vec.z > pitch_length && mBallState.owner == Away))
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
                                    mBallState.restart_point.x = pitch_width / 2.0f - 7.5f;
                                else
                                    mBallState.restart_point.x = pitch_width / 2.0f + 7.5f;
                            }
                            else if (mBallState.bio_type == Cornerkick)
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
                            else    // Kickoff
                            {
                                mBallState.restart_point.x = pitch_width / 2.0f;
                                mBallState.restart_point.z = pitch_length / 2.0f;
                            }
                        }
                    }
                }
            }

            bool Rules::play_can_be_given_free() const
            {
                typedef std::map<int, boost::shared_ptr<MatchPlayer> > EntityMap;
                std::map<int, boost::shared_ptr<MatchPlayer> > entitymap;
                mMatchStatus->getPlayers(entitymap);
                std::vector<int>::const_iterator it;
                switch (mBallState.bio_type)
                {
                    case Throwin:
                        if(ball_due_for_throwin(mMatchStatus->getBall()->getPosition()))
                            return false;
                        // fall through
                    case Goalkick:
                    case Cornerkick:
                        if(ball_over_a_goal_line(mMatchStatus->getBall()->getPosition()))
                            return false;
                    case IndirectFreekick:
                    case DirectFreekick:
                    case PenaltyKick:
                    case DroppedBall:
                    case PreKickoff:
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
                                    {
                                        substitute_on_pitch = true;
                                    }
                                }
                            }
                        }
                        return (!substitute_on_pitch);
                        break;
                    }
                    case Kickoff:
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
                        return (!player_not_on_his_side);
                        break;
                    }
                    case BallIn:
                        return true;
                    case HalfFullTime:
                    default:
                        return false;
                        break;
                }
                return false;
            }

            void Rules::check_for_bio_change(bool& new_ball_status, const bool& ball_touched)
            {
                if(mBallState.bio_type == PreKickoff && !mBallState.blocked_play)
                {
                    mBallState.bio_type = Kickoff;
                    mBallState.blocked_play = true;
                }
                if(mBallState.bio_type != BallIn)
                {
                    bool can_give_free = play_can_be_given_free();
                    if(can_give_free)
                    {
                        if(mBallState.blocked_play)
                        {
                            std::cerr << __func__ << ": can give free and blocked; unblocking.\n";
                            new_ball_status = true;
                            mBallState.blocked_play = false;
                            if(ball_touched)
                            {
                                std::cerr << __func__ << ": ball touched.\n";
                                mBallState.nextBallInOut();
                            }
                        }
                        if(!mBallState.blocked_play && ball_touched)
                        {
                            std::cerr << __func__ << ": can give free, not blocked, now touched.\n";
                            mBallState.bio_type = BallIn;
                            mBallState.blocked_play = false;
                            new_ball_status = true;
                        }
                    }
                    else
                    {
                        if(!mBallState.blocked_play)
                        {
                            std::cerr << __func__ << ": can not give free, not blocked; blocking.\n";
                            mBallState.blocked_play = true;
                            new_ball_status = true;
                        }
                    }
                }
            }

            void Rules::update(Physics* p)
            {
                PhysicsMessageList l;
                OwnerMessageList c;
                p->getUpdates(l, c);

                bool new_ball_status = false;
                bool ball_touched = false;
                GoalQuery goal_scored = NoGoal;

                if(!(mBallState.bio_type == PreKickoff || mBallState.bio_type == HalfFullTime))
                {
                    check_for_touched_ball(c, new_ball_status, ball_touched);
                    check_for_over_pitch(l, new_ball_status, goal_scored);
                }

                check_for_bio_change(new_ball_status, ball_touched);

                boost::posix_time::ptime this_time(boost::posix_time::microsec_clock::local_time());
                boost::posix_time::time_period diff_time(last_update_time, this_time);
                boost::posix_time::time_duration diff_dur = diff_time.length();
                unsigned long us_diff = diff_dur.total_microseconds();
                if(us_diff > update_time_interval_ms) new_ball_status = true;

                if(new_ball_status)
                {
                    messages::GeneralUpdateStatusMessage rm(mBallState);
                    newmessages.push_back(rm);
                }

                if(goal_scored != NoGoal)
                {
                    messages::GeneralUpdateScoreMessage rm(mMatchStatus->getHomeScore(), mMatchStatus->getAwayScore(), 0, 0);
                    // TODO: also use penalty kick score; also see matchstatus.update
                    newscores.push_back(rm);
                }

                if(newmessages.size() > 0 || newscores.size() > 0)
                {
                    last_update_time = this_time;
                    publish();
                    mMatchStatus->update(newmessages);
                    mMatchStatus->update(newscores);
                    clearMessages();
                }
            }

            void Rules::getUpdates (RulesMessageList& pes, ScoreMessageList& scs) const
            {
                pes = newmessages;
                scs = newscores;
            }

            void Rules::clearMessages()
            {
                newmessages.clear();
                newscores.clear();
            }
        }
    }
}

