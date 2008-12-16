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
            Dispatcher::Dispatcher (ClientListPtr clp, 
                                    addutil::network::Server* s, 
                                    boost::shared_ptr<Physics> p, 
                                    boost::shared_ptr<Rules> r,
                                    boost::shared_ptr<MatchStatus> ms)
                : mClientList(clp),
                  srv(*s),
                  mPhysics(p),
                  mRules(r),
                  mMatchStatus(ms)
            {
                mPhysics->subscribe(*this);
                mRules->subscribe(*this);
                // boost::thread t(boost::bind(&Dispatcher::run, this));
            }

            Dispatcher::~Dispatcher ( ) 
            { 
                mPhysics->unsubscribe(*this);
                mRules->unsubscribe(*this);
            }

            void Dispatcher::run()
            {
                while(1)
                {
                    sleep(1);
                    // srv.broadcast("kpa\n");
                }
            }

            void Dispatcher::update(Physics* p)
            {
                PhysicsMessageList l;
                p->getUpdates(l);
                dispatchPhysicsMessages(l);
            }

            void Dispatcher::update(Rules* r)
            {
            }

            void Dispatcher::dispatchClientInformation ( ) 
            {

            }

            void Dispatcher::dispatchConnectionMessage (const ConnectionMessage& e ) 
            {

            }

            void Dispatcher::dispatchPhysicsMessage (const PhysicsMessage& e ) 
            {

            }

            void Dispatcher::dispatchRulesMessage (const RulesMessage& e ) 
            {

            }

            void Dispatcher::dispatchPhysicsState (const PhysicsState& s ) 
            {

            }

            void Dispatcher::dispatchRulesState (const RulesState& s ) 
            {

            }

            void Dispatcher::dispatchPhysicsMessages (const PhysicsMessageList& es ) 
            {
                // TODO: save and/or dispatch
                // default should be dispatch,
                // for efficiency only every x ms should a dispatch
                // occur, otherwise save updated entities internally
                // (in dispatcher) and dispatch everything later

                std::ostringstream oss(std::ostringstream::out);
                BOOST_FOREACH(PhysicsMessage m, es)
                {
                    oss << m.toString();
                }
                oss << "\n";
                srv.multicast(oss.str(), 1);  // TODO: enum instead of 1 (gid)
            }

            void Dispatcher::dispatchRulesMessages (const RulesMessageList& es ) 
            {

            }

            void Dispatcher::dispatchConnectionMessages (const ConnectionMessageList& es ) 
            {

            }

            void Dispatcher::newClientMessage(unsigned int clid, const messages::InitialDataRequest& m)
            {
                messages::InitialDataMessage idm(*mMatchStatus);
                try
                {
                    srv.write(idm.toString(), clid);
                }
                catch (...)
                {
                    std::cerr << "Dispatcher::newClientMessage: Trying to write to a non-existing Client ID\n";
                }
            }
        }
    }
}

