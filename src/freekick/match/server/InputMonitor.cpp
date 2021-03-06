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

#include "InputMonitor.h"

namespace freekick
{
    namespace match
    {
        namespace server
        {
            InputMonitor::InputMonitor (boost::shared_ptr<MatchStatus> ms)
                : mMatchStatus(ms),
                  mSetpoints(new SetpointMap()),
                  mVelocities(new VelocityMap()),
                  mKicks(new KickMap())
            {
            }

            void InputMonitor::newClientMessage (const messages::MovePlayerControlMessage& e)
            {
                addutil::Vector3 v;
                e.getTargetVector(v);
                int plid = e.getPlayerID();

                v.capY(maxJumpVelocity);
                // if(player_on_ground)   // TODO: define
                    (*mVelocities)[plid].y = v.y;

                int plspeed_skill = 0;
                try
                {
                    plspeed_skill = mMatchStatus->getPlayer(plid)->playerskills.speed;
                } catch(...)
                {
                    std::cerr << "InputMonitor:" << __func__ << ": move player control message from a non-existing player?\n";
                    return;
                }

                // TODO: define max/min velocity somewhere else and take stamina etc. into account
                if(v.length() > 10.0f)
                {
                    v.normalize();
                    v *= (5.0f + (plspeed_skill / 200.0f));
                }

                (*mSetpoints)[plid] = v;

                VelocityMap::iterator itv = mVelocities->find(plid);
                if(itv == mVelocities->end())
                    mVelocities->insert(std::pair<int, addutil::Vector3>(plid, Vector3(0.0f, 0.0f, 0.0f)));

                if(mMatchStatus->holdingBall() == plid)
                {
                    std::cerr << "InputMonitor: client holding the ball moved (not holding the ball anymore).\n";
                    mMatchStatus->setBallHolder(0);
                }
            }

            void InputMonitor::newClientMessage (const messages::KickPlayerControlMessage& e)
            {
                addutil::Vector3 v;
                e.getTargetVector(v);
                int plid = e.getPlayerID();
                boost::shared_ptr<MatchPlayer> pl;

                try
                {
                    pl = mMatchStatus->getPlayer(plid);
                }
                catch(const char* e)
                {
                    // player not in matchstatus
                    std::cerr << "InputMonitor::newClientMessage: " << e << std::endl;
                    return;
                }
                addutil::Vector3 dist = pl->getPosition() - mMatchStatus->getBall()->getPosition();

                // TODO: also check ball height vs. feet
                if(dist.length() > constants::max_kick_distance)
                {
                    // std::cerr << "InputMonitor: Player tried to kick but was too far away\n";
                    return;
                }

                if(!pl->timerCanKick())
                {
                    std::cerr << __func__ << ": player timer not at 0 yet" << std::endl;
                    return;
                }
                // TODO: define max. kick velocity somewhere else and take stamina, current player velocity, etc. into account
                if(v.length() > 30.0f)
                {
                    v.normalize();
                    v *= 30.0f;
                }

                bool passing = v.normalized().y < 0.12f;

                using addutil::general::rand_float;
                if(passing)
                {   // TODO: define constants somewhere else
                    int plpass_skill = pl->playerskills.passing;
                    float plpass_percent = plpass_skill * 0.001f;
                    float max_easy_pass_len = 15.0f * plpass_percent;
                    float max_possible_pass_len = 5.0f + 25.0f * plpass_percent;
                    float pass_len = v.length();
                    if(pass_len > max_possible_pass_len)
                    {
                        v.normalize();
                        v *= max_possible_pass_len;
                        pass_len = max_possible_pass_len;
                    }
                    float max_variation;
                    if(pass_len < max_easy_pass_len)
                        max_variation = pass_len * 0.1f;
                    else
                        max_variation = pass_len * 0.3f;
                    if(variateDueToMovingBall(v))
                    {
                        max_variation *= 2.0f;
                    }
                    v.x += rand_float(-max_variation, max_variation);
                    v.z += rand_float(-max_variation, max_variation);
                }
                else
                {
                    int plshoot_skill = pl->playerskills.shooting;
                    int placc_skill = pl->playerskills.accuracy;
                    float plshoot_percent = plshoot_skill * 0.001f;
                    float placc_percent = placc_skill * 0.001f;
                    float max_easy_shot_len = 15.0f * placc_percent;
                    float max_possible_shot_len = 10.0f + 20.0f * plshoot_percent;
                    float shot_len = v.length();
                    if(shot_len > max_possible_shot_len)
                    {
                        v.normalize();
                        v *= max_possible_shot_len;
                        shot_len = max_possible_shot_len;
                    }
                    float max_variation;
                    if(shot_len < max_easy_shot_len)
                        max_variation = shot_len * 0.05f;
                    else
                        max_variation = shot_len * 0.35f;
                    if(variateDueToMovingBall(v))
                    {
                        max_variation *= 2.0f;
                    }
                    v.x += rand_float(-max_variation, max_variation);
                    v.y += rand_float(-max_variation, max_variation);
                    if(v.y < 0.0f)
                        v.y = 0.0f;
                    v.z += rand_float(-max_variation, max_variation);
                }

                pl->setKickTimer(constants::kick_timer);
                (*mKicks)[plid] = v;

                if(mMatchStatus->holdingBall() == plid)
                {
                    std::cerr << "InputMonitor: client holding the ball kicked the ball away.\n";
                    mMatchStatus->setBallHolder(0);
                }
            }

            bool InputMonitor::variateDueToMovingBall(const Vector3& kickvec) const
            {
                if(kickvec.length2() < 8.0f) 
                    return false;
                const addutil::Vector3& ballvec = mMatchStatus->getBall()->getVelocity();
                if(ballvec.length2() < 8.0f)
                    return false;
                float ang = kickvec.angleBetweenXZ(ballvec);
                return (abs(ang) < addutil::pi_4 || abs(ang) > addutil::pi_3_4);
            }

            void InputMonitor::newClientMessage (const messages::HoldPlayerControlMessage& e)
            {
                addutil::Vector3 v;
                e.getTargetVector(v);
                messages::MovePlayerControlMessage m(e.getPlayerID(), v);
                newClientMessage(m);

                boost::shared_ptr<MatchPlayer> pl;
                int plid = e.getPlayerID();
                try
                {
                    pl = mMatchStatus->getPlayer(plid);
                }
                catch(const char* e)
                {
                    std::cerr << "InputMonitor::newClientMessage: " << e << std::endl;
                    return;
                }
                float dist = (pl->getPosition() - mMatchStatus->getBall()->getPosition()).length();
                if(dist < 1.5f)           // TODO: define this somewhere else (and make it more realistic)
                {
                    std::cerr << "InputMonitor: Player " << e.getPlayerID() << " holds the ball\n";
                    mMatchStatus->setBallHolder(e.getPlayerID());
                }
                else std::cerr << "InputMonitor: Player tried to hold ball but was too far away\n";
            }

            void InputMonitor::interpolate(unsigned long microseconds)
            {
                SetpointMap::iterator itv = mVelocities->begin();
                SetpointMap::iterator its = mSetpoints->begin();
                while(its != mSetpoints->end())
                {
                    itv = mVelocities->find(its->first);
                    if(itv == mVelocities->end()) continue;
                    addutil::Vector3& should = its->second;
                    addutil::Vector3& is = itv->second;
                    interpolate(microseconds, should.x, is.x);
                    // interpolate(microseconds, should.y, is.y);
                    interpolate(microseconds, should.z, is.z);
                    its++;
                }
            }

            void InputMonitor::interpolate(unsigned long microseconds, const float& should, float& is)
            {
                if(should == is) return;

                float diff = std::fabs(should - is);
                if(diff < 0.001)
                {
                    is = should;
                    return;
                }

                // TODO: define acceleration constant (below) elsewhere
                float add = std::min(diff * (microseconds / 200000.0f), diff);
                if(is < should)
                    is += add;
                else
                    is -= add;
            }

            boost::shared_ptr<VelocityMap> InputMonitor::getVelocities()
            {
                return mVelocities;
            }

            boost::shared_ptr<KickMap> InputMonitor::getKicks()
            {
                return mKicks;
            }
        }
    }
}
