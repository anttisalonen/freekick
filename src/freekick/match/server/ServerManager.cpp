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

#include "ServerManager.h"

namespace freekick
{
    namespace match
    {
        namespace server
        {
            ServerManager::ServerManager(int port)
                : clients(new ClientList()),
                  cel(clients),
                  d(clients, this)
            {
                std::cerr << "Starting listening on port " << port << std::endl;
                startListening(port);
            }

            ServerManager::~ServerManager()
            {
                clients->clear();
                std::cerr << "Stopping listening.\n";
                stopListening();
            }

            void ServerManager::client_connected(client_id id)
            {
                std::cerr << "Client " << id << " connected.\n";
                write("Welcome to the Freekick server\n", id);
                (*clients)[id] = Client(id);
            }

            void ServerManager::client_disconnected(client_id id)
            {
                clients->erase(clients->find(id));
                std::cerr << "Client " << id << " disconnected.\n";
            }

            void ServerManager::client_input(client_id id, const std::string& msg)
            {
                cel.newData(id, msg);
            }
        }
    }
}
