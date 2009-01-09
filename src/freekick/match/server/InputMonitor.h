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


#ifndef INPUTMONITOR_H
#define INPUTMONITOR_H

#include <map>
#include <algorithm>
#include <cmath>

#include <boost/shared_ptr.hpp>

#include "addutil/Vector3.h"

#include "MatchStatus.h"
#include "messages/MovePlayerControlMessage.h"
#include "messages/KickPlayerControlMessage.h"

namespace freekick
{
    namespace match
    {
        namespace server
        {
            // 1. a map that contains the setpoints and real velocities of all players.
            // the velocities are compared and the real velocity is updated to near the
            // setpoint with each interpolate(). the setpoints are updated in case of
            // a newclientmessage().
            // 2. a map that contains the real velocities of all players.
            // this map is given to physics to update the velocities of the moving bodies.
            // this map (velocitymap) must be kept synchronised with the first map
            // (setpointmap). in addition, physics will change the values of the real
            // real velocities in velocitymap.
            typedef std::map<int, addutil::Vector3> SetpointMap;
            typedef std::map<int, addutil::Vector3> VelocityMap;

            typedef std::map<int, addutil::Vector3> KickMap;

            class InputMonitor
            {
            public:
                InputMonitor(boost::shared_ptr<MatchStatus> ms);
                virtual ~InputMonitor() { }
                void newClientMessage (const messages::MovePlayerControlMessage& e);
                void newClientMessage (const messages::KickPlayerControlMessage& e);
                void interpolate(unsigned long microseconds);
                boost::shared_ptr<VelocityMap> getVelocities();
                boost::shared_ptr<KickMap> getKicks();

            protected:
                void interpolate(unsigned long microseconds, const float& should, float& is);

            private:
                boost::shared_ptr<MatchStatus> mMatchStatus;
                boost::shared_ptr<SetpointMap> mSetpoints;
                boost::shared_ptr<VelocityMap> mVelocities;
                boost::shared_ptr<KickMap> mKicks;
                static const float maxJumpVelocity = 5.0f;
            };
        }
    }
}

#endif
