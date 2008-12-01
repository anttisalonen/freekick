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

#include "addutil/network/Server.h"

#include "freekick/match/Client.h"
#include "ConnectionListener.h"
#include "ClientEventListener.h"
#include "Dispatcher.h"

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
                ServerManager(int port);
                virtual ~ServerManager();
                void client_connected(client_id id);
                void client_disconnected(client_id id);
                void client_input(client_id id, const std::string& msg);
            private:
                ClientListPtr clients;
                ClientEventListener cel;
                Dispatcher d;
            };
        }
    }
}

#endif // SERVERMANAGER_H
