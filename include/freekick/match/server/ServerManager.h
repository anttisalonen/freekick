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


#ifndef SERVERMANAGER_H
#define SERVERMANAGER_H

#include <string>

#include <boost/shared_ptr.hpp>
#include <boost/thread.hpp>
#include <boost/bind.hpp>

#include "addutil/network/Server.h"

#include "freekick/match/Client.h"
#include "freekick/match/MatchStatus.h"
#include "ClientEventListener.h"
#include "Dispatcher.h"
#include "Rules.h"
#include "Physics.h"
#include "messages/ClientInitMessage.h"
#include "messages/ServerInitMessage.h"

/**
 * class ServerManager
 */

namespace freekick
{
    namespace match
    {
        namespace server
        {
            typedef std::map<unsigned int, freekick::match::Client> ClientList;
            typedef boost::shared_ptr<ClientList> ClientListPtr;
            using addutil::network::client_id;

            class ServerManager : public addutil::network::Server
            {
            public:
                ServerManager(unsigned int port, 
                              boost::shared_ptr<freekick::match::MatchStatus> ms,
                              const std::string& servername,
                              const std::string& greeting);
                virtual ~ServerManager();
                void client_connected(client_id id);
                void client_disconnected(client_id id);
                void client_input(client_id id, std::string& msg);
                bool run();
            private:
                unsigned int mPort;
                ClientListPtr clients;
                boost::shared_ptr<Physics> p;
                boost::shared_ptr<Rules> r;
                boost::shared_ptr<Dispatcher> d;
                boost::shared_ptr<ClientEventListener> cel;
                std::string name;
                std::string greet;
                std::string protocol_version;
            };
        }
    }
}

#endif // SERVERMANAGER_H
