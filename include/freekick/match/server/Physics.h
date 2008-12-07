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

#include <boost/shared_ptr.hpp>
#include <boost/date_time/posix_time/posix_time.hpp>

#include "ClientEvent.h"
#include "Dispatcher.h"
#include "PhysicsState.h"
#include "PhysicsEngine.h"
#include "BulletPhysicsEngine.h"
#include "Rules.h"
#include "MatchStatus.h"

namespace freekick
{
    namespace match
    {
        namespace server
        {
            class Physics
            {
            public:
                Physics (boost::shared_ptr<Dispatcher> d, boost::shared_ptr<Rules> r, boost::shared_ptr<MatchStatus> ms);
                virtual ~Physics ( );

                bool run ( );
                void newClientEvent (freekick::match::ClientEvent e );

                PhysicsState getPhysicsState ( );
                void setPause (bool p );
                bool getPause ( );
                void setNewPhysicsEvents ( PhysicsEventList new_var );
                PhysicsEventList getNewPhysicsEvents ( );

            private:

                bool mPause;
                PhysicsEventList mNewPhysicsEvents;
                PhysicsState mPhysicsState;
                boost::shared_ptr<Dispatcher> mDispatcher;
                boost::shared_ptr<Rules> mRules;
                boost::shared_ptr<MatchStatus> mMatchStatus;
                boost::shared_ptr<PhysicsEngine> mPhysicsEngine;

            };
        }
    }
}

#endif // PHYSICS_H
