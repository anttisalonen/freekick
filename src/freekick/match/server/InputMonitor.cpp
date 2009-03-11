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

                // TODO: define max. velocity somewhere else and take stamina etc. into account
                if(v.length() > 10.0f)
                {
                    v.normalize();
                    v *= 10.0f;
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

                // TODO: define max. kick velocity somewhere else and take stamina, current player velocity, etc. into account
                if(v.length() > 30.0f)
                {
                    v.normalize();
                    v *= 30.0f;
                }

                (*mKicks)[plid] = v;

                if(mMatchStatus->holdingBall() == plid)
                {
                    std::cerr << "InputMonitor: client holding the ball kicked the ball away.\n";
                    mMatchStatus->setBallHolder(0);
                }
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
