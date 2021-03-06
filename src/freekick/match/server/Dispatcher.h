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
  This file was generated on Sa Nov 22 2008 at 11:40:55
**************************************************************************/


#ifndef DISPATCHER_H
#define DISPATCHER_H

#include <boost/thread.hpp>
#include <boost/bind.hpp>
#include <boost/shared_ptr.hpp>
#include <boost/date_time/posix_time/posix_time.hpp>

#include "addutil/network/Server.h"
#include "addutil/Reader.h"
#include "addutil/Color.h"
#include "addutil/General.h"

#include "freekick/match/Client.h"
#include "Physics.h"
#include "Rules.h"
#include "MatchData.h"

#include "messages/Messages.h"

namespace freekick
{
    namespace match
    {
        namespace server
        {
            typedef messages::Message ConnectionMessage;
            typedef std::vector<ConnectionMessage> ConnectionMessageList;
            class Dispatcher : public addutil::Reader<Physics>, public addutil::Reader<Rules>
            {
            public:

                Dispatcher (ClientListPtr clp, 
                            addutil::network::Server* s, 
                            boost::shared_ptr<Physics> p, 
                            boost::shared_ptr<Rules> r,
                            boost::shared_ptr<MatchStatus> ms);
                virtual ~Dispatcher ( );

                void update(Physics* p);
                void update(Rules* r);

                void dispatchClientInformation ( );
                template <typename T> void dispatchMessages(const std::vector<T>& ms)
                {
                    if(ms.size() < 1) return;
                    std::ostringstream oss(std::ostringstream::out);
                    typename std::vector<T>::const_iterator it;
                    for(it = ms.begin(); it != ms.end(); ++it)
                    {
                        oss << it->toString();
                    }
                    oss << "\n";
                    srv.multicast(oss.str(), 1);  // TODO: enum instead of 1 (gid)
                }
                void dispatchPhysicsMessages (const PhysicsMessageList& es );
                void newClientMessage(unsigned int clid, const messages::InitialDataRequest& m);   // TODO: add handlers for all receivable messages
                void newClientMessage(unsigned int clid, const messages::SetGeneralUpdateIntervalMessage& m);
                void newClientMessage(unsigned int clid, const messages::SetConstantUpdateIntervalMessage& m);
                void newClientMessage(unsigned int clid, const messages::GetGeneralUpdateInterval& m);
                void newClientMessage(unsigned int clid, const messages::GetConstantUpdateInterval& m);
                void sendPlayerList(unsigned int clid);

            private:
                ClientListPtr mClientList;
                addutil::network::Server& srv;
                void run();
                boost::shared_ptr<Physics> mPhysics;
                boost::shared_ptr<Rules> mRules;
                boost::shared_ptr<MatchStatus> mMatchStatus;
                boost::posix_time::ptime last_physics_dispatch_time;

                static const int min_dispatch_interval = 50;
                static const int dispatch_interval_step = 50;
                static const int max_dispatch_interval = 10000;
            };
        }
    }
}

#endif // DISPATCHER_H
