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

#include "Dispatcher.h"

// Constructors/Destructors
//  

namespace freekick
{
    namespace match
    {
        namespace server
        {
            Dispatcher::Dispatcher (ClientListPtr clp, addutil::network::Server* s)
                : mClientList(clp),
                  srv(*s)
            {
                boost::thread t(boost::bind(&Dispatcher::run, this));
            }

            Dispatcher::~Dispatcher ( ) 
            { 
            }

            void Dispatcher::run()
            {
                while(1)
                {
                    sleep(1);
                    // srv.broadcast("kpa\n");
                }
            }

            void Dispatcher::dispatchClientInformation ( ) 
            {

            }

            void Dispatcher::dispatchConnectionEvent (ConnectionEvent e ) 
            {

            }

            void Dispatcher::updatePhysics(EntityPtrMap m)
            {
                // TODO: save and/or dispatch
                // default should be dispatch,
                // for efficiency only every x ms should a dispatch
                // occur, otherwise save updated entities internally
                // (in dispatcher) and dispatch everything later

                std::ostringstream oss(std::ostringstream::out);
                typedef std::pair<ObjectID, EntityPtr> pair_en;
                BOOST_FOREACH(pair_en p, m)
                {
                    const addutil::Vector3 pos = p.second->getPosition();
                    const addutil::Quaternion orien = p.second->getOrientation();
                    messages::ConstantUpdateMessage c(p.first, 0, pos, orien);
                    oss << c.toString();
                }
                oss << "\n";
                srv.multicast(oss.str(), 1);  // TODO: enum instead of 1 (gid)
            }

            /*
              void Dispatcher::dispatchPhysicsEvent (PhysicsEvent e ) 
              {

              }

              void Dispatcher::dispatchRulesEvent (RulesEvent e ) 
              {

              }

              void Dispatcher::dispatchPhysicsState (PhysicsState s ) 
              {

              }

              void Dispatcher::dispatchRulesState (RulesState s ) 
              {

              }

              void Dispatcher::dispatchPhysicsEvents (PhysicsEventList es ) 
              {

              }

              void Dispatcher::dispatchRulesEvents (RulesEventList es ) 
              {

              }

              void Dispatcher::dispatchConnectionEvents (ConnectionEventList es ) 
              {

              }
            */
        }
    }
}

