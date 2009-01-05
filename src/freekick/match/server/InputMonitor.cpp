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

#include "InputMonitor.h"

namespace freekick
{
    namespace match
    {
        namespace server
        {
            InputMonitor::InputMonitor (boost::shared_ptr<MatchStatus> ms)
                : mMatchStatus(ms)
            {
            }

            void InputMonitor::newClientMessage (const messages::MovePlayerControlMessage& e)
            {
                addutil::Vector3 v;
                e.getTargetVector(v);
                int plid = e.getPlayerID();

                v.capY(maxJumpVelocity);
                // if(player_on_ground)   // TODO: define
                    mVelocities[plid].y = v.y;

                // TODO: define max. velocity somewhere else and take stamina etc. into account
                if(v.length() > 10.0f)
                {
                    v.normalize();
                    v *= 10.0f;
                }

                mSetpoints[plid] = v;

                VelocityMap::iterator itv = mVelocities.find(plid);
                if(itv == mVelocities.end())
                    mVelocities.insert(std::pair<int, addutil::Vector3>(plid, Vector3(0.0f, 0.0f, 0.0f)));
            }

            void InputMonitor::interpolate(unsigned long microseconds)
            {
                SetpointMap::iterator itv = mVelocities.begin();
                SetpointMap::iterator its = mSetpoints.begin();
                while(its != mSetpoints.end())
                {
                    itv = mVelocities.find(its->first);
                    if(itv == mVelocities.end()) continue;
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

                float add = std::min(diff * (microseconds / 50000.0f), diff);
                if(is < should)
                    is += add;
                else
                    is -= add;
            }

            VelocityMap& InputMonitor::getVelocities()
            {
                return mVelocities;
            }
        }
    }
}
