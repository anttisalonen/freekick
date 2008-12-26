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


#ifndef PHYSICS_H
#define PHYSICS_H

#include <list>
#include <vector>

#include <boost/tuple/tuple.hpp>
#include <boost/shared_ptr.hpp>
#include <boost/date_time/posix_time/posix_time.hpp>

#include "Reader.h"
#include "Publisher.h"
#include "Color.h"

#include "PhysicsEngine.h"
#include "BulletPhysicsEngine.h"
#include "MatchStatus.h"
#include "MatchIDs.h"
#include "InputMonitor.h"

#include "messages/MovePlayerControlMessage.h"
#include "messages/ConstantUpdateMessage.h"

namespace freekick
{
    namespace match
    {
        namespace server
        {
            typedef messages::ConstantUpdateMessage PhysicsMessage;
            typedef std::vector<PhysicsMessage> PhysicsMessageList;

            // TODO: fill collision list from collision callbacks for rules
            typedef boost::tuple<int, int, float> Collision;   // Player IDs of the collided objects + collision power
            typedef std::vector<Collision> CollisionList;

            class Physics : public Reader<PhysicsEngine>, public Publisher<Physics>
            {
            public:
                Physics (boost::shared_ptr<MatchStatus> ms, boost::shared_ptr<InputMonitor> im);
                virtual ~Physics ( );

                bool run ( );
                void setPause (bool p );
                bool getPause ( );

                // Reader<PhysicsEngine>
                void update(PhysicsEngine* e);

                // Publisher<Physics>
                void getUpdates(PhysicsMessageList& l) const;
                void getUpdates(PhysicsMessageList& l, CollisionList& c) const;

            protected:
                // Publisher<Physics>
                void clearMessages();

            private:
                bool mPause;

                boost::shared_ptr<MatchStatus> mMatchStatus;
                boost::shared_ptr<PhysicsEngine> mPhysicsEngine;

                // InputMonitor delivers the messages to Physics
                boost::shared_ptr<InputMonitor> mInputMonitor;

                // Messages that will be published are stored here
                PhysicsMessageList newmessages;
                CollisionList newcollisions;

                const static float collision_box_height = 1.75f;
            };
        }
    }
}

#endif // PHYSICS_H
