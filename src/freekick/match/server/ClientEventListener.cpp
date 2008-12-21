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

#include "ClientEventListener.h"

// Constructors/Destructors
//  

namespace freekick
{
    namespace match
    {
        namespace server
        {
            ClientEventListener::ClientEventListener (ClientListPtr clp, 
                                                      boost::shared_ptr<InputMonitor> im, 
                                                      boost::shared_ptr<Dispatcher> d)
                : mClientList(clp),
                  mInputMonitor(im),
                  mDispatcher(d)
            {
            }

            ClientEventListener::~ClientEventListener ( ) 
            { 
            }

            void ClientEventListener::newData (unsigned int clientid, buffer b ) 
            {
                using namespace messages;

                std::string t;
                try
                {
                    t = getMessageType(b);
                }
                catch(...)
                {
                    std::cerr << "ClientEventListener: received invalid message.\n";
                    return;
                }

                ClientList::iterator it = mClientList->find(clientid);
                if(it == mClientList->end())
                {
                    std::cerr << "ClientEventListener: client not in the client list? (Should never happen.) Adding.\n";
                    (*mClientList)[clientid] = Client(clientid);
                    return;
                }

                // MovePlayerControlMessage: handled by InputMonitor.
                if (t == c_pl_ctl_move)
                {
                    try
                    {
                        const messages::MovePlayerControlMessage m(b);
                        unsigned int playerid = m.getPlayerID();
                        if(!(*it).second.controlsPlayer(playerid))
                        {
                            std::cerr << "ClientEventListener: client " << clientid << " trying to control another player (" << playerid << ").\n";
                            return;
                        }
                        mInputMonitor->newClientMessage(m);
                    }
                    catch(...)
                    {
                        std::cerr << "ClientEventListener: failed to parse MovePlayerControlMessage.\n";
                    }
                    return;
                }

                // Initial Data Request: handled by Dispatcher.
                else if (t == c_initial_data_req)
                {
                    const messages::InitialDataRequest m;
                    mDispatcher->newClientMessage(clientid, m);
                    return;
                }

                // Player Control Request: handled by Client Event Listener.
                else if (t == c_pl_cont_req)
                {
                    try
                    {
                        const messages::PlayerControlRequestMessage m(b);
                        // TODO: check if players already reserved
                        // Be sure the ServerManager thread doesn't change the client at the same time...
                        std::set<int> pls;
                        m.getPlayers(pls);
                        (*it).second.clearPlayers();
                        (*it).second.addPlayers(pls);
                    }
                    catch(const char* c)
                    {
                        std::cerr << "ClientEventListener: failed to parse PlayerControlRequestMessage: " << c << "\n";
                    }
                    catch(...)
                    {
                        std::cerr << "ClientEventListener: failed to parse PlayerControlRequestMessage.\n";
                    }
                }

                // TODO: add handling for all messages
            }
        }
    }
}

