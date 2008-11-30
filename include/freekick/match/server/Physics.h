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


#include "ClientEvent.h"
#include "Dispatcher.h"
#include "PhysicsState.h"
#include "PhysicsEngine.h"


namespace freekick
{
    namespace match
    {
        namespace server
        {
            class Physics
            {
            public:
                Physics ( );
                virtual ~Physics ( );
                void setPause (bool p );
                void run ( );
                void newClientEvent (freekick::match::ClientEvent e );
                bool stepPhysics ( );

            private:

                // Private attributes
                //  

                bool mPause;
                PhysicsEventList mNewPhysicsEvents;
                PhysicsEngine mPhysicsEngine;
                PhysicsState mPhysicsState;
                boost::shared_ptr<Dispatcher> mDispatcher;

            public:

                // Private attribute accessor methods
                //  

                bool getPause ( );
                void setNewPhysicsEvents ( PhysicsEventList new_var );
                PhysicsEventList getNewPhysicsEvents ( );
                void setPhysicsEngine ( PhysicsEngine new_var );
                PhysicsEngine getPhysicsEngine ( );
                void setPhysicsState ( PhysicsState new_var );
                PhysicsState getPhysicsState ( );

            };
        }
    }
}

#endif // PHYSICS_H
