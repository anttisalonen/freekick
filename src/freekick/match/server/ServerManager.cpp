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
            ServerManager::ServerManager(unsigned int port, 
                                         boost::shared_ptr<freekick::match::MatchStatus> ms,
                                         const std::string& servername,
                                         const std::string& greeting)
                : mPort(port), 
                  clients(new ClientList()),
                  im(new InputMonitor(ms)),
                  p(new Physics(ms, im)),
                  r(new Rules(ms, p)),
                  d(new Dispatcher(clients, this, p, r, ms)),
                  cel(new ClientEventListener(clients, im, d)),
                  name(servername),
                  greet(greeting),
                  protocol_version("0.2")
            {
            }

            ServerManager::~ServerManager()
            {
                clients->clear();
                std::cerr << "Stopping listening.\n";
                stopListening();
            }

            bool ServerManager::run()
            {
                std::cerr << "Starting physics\n";
                boost::thread physics_thread(boost::bind(&freekick::match::server::Physics::run, p));
                std::cerr << "Starting listening on port " << mPort << std::endl;
                startListening(mPort);
                physics_thread.join();
                return true;
            }

            void ServerManager::client_connected(client_id id)
            {
                std::cerr << "Client " << id << " connected.\n";
                messages::ServerInitMessage sim(name, protocol_version, greet);
                write(sim.toString(), id);
            }

            void ServerManager::client_disconnected(client_id id)
            {
                ClientList::iterator it;
                it = clients->find(id);
                std::cerr << "Client " << id << " disconnected.\n";
                if (it != clients->end())
                {
                    clients->erase(it);
                }
                remove_from_group(id, 1);
            }

            void ServerManager::client_input(client_id id, std::string& msg)
            {
                ClientList::iterator it;
                it = clients->find(id);
                if (it != clients->end())
                {
                    cel->newData(id, msg);
                    return;
                }
                else    // handshake
                {
                    std::cout << "Handshake\n";
                    try
                    {
                        messages::ClientInitMessage cim(msg);
                        std::set<int> ps;
                        // TODO: check if players already reserved
                        cim.getPlayers(ps);
                        Client c(id);
                        (*clients)[id] = c;
                        c.addPlayers(ps);
                        add_to_group(id, 1);    // TODO: enum instead of 1 (gid)
                    }
                    catch (const char* s)
                    {
                        std::cerr << "Could not parse ClientInitMessage: " << s << std::endl;
                        disconnect(id);
                        return;
                    }
                    catch (...)
                    {
                        std::cerr << "Could not parse ClientInitMessage" << std::endl;
                        disconnect(id);
                        return;
                    }
                }
            }
        }
    }
}
