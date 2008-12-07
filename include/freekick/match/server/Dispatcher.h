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


#ifndef DISPATCHER_H
#define DISPATCHER_H

#include <boost/thread.hpp>
#include <boost/bind.hpp>

#include "addutil/network/Server.h"

#include "freekick/match/Client.h"
#include "RulesState.h"
#include "RulesEvent.h"
#include "PhysicsState.h"
#include "PhysicsEvent.h"
#include "PhysicsReader.h"

#include "ConnectionEvent.h"
#include "messages/ConstantUpdateMessage.h"

namespace freekick
{
    namespace match
    {
        namespace server
        {
            typedef std::vector<PhysicsEvent> PhysicsEventList;
            typedef std::vector<RulesEvent> RulesEventList;
            typedef std::vector<ConnectionEvent> ConnectionEventList;
            class Dispatcher : public PhysicsReader
            {
            public:

                Dispatcher (ClientListPtr clp, addutil::network::Server* s);
                virtual ~Dispatcher ( );

                void dispatchClientInformation ( );
                void dispatchConnectionEvent (freekick::match::ConnectionEvent e );
                /*
                void dispatchPhysicsEvent (freekick::match::PhysicsEvent e );
                void dispatchRulesEvent (freekick::match::RulesEvent e );
                void dispatchPhysicsState (freekick::match::PhysicsState s );
                void dispatchRulesState (freekick::match::RulesState s );
                void dispatchPhysicsEvents (PhysicsEventList es );
                void dispatchRulesEvents (RulesEventList es );
                void dispatchConnectionEvents (ConnectionEventList es );
                */
                void updatePhysics(EntityPtrMap m);

            private:
                ClientListPtr mClientList;
                addutil::network::Server& srv;
                void run();

            };
        }
    }
}

#endif // DISPATCHER_H
