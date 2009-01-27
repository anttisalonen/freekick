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

            void ClientEventListener::newData (int clientid, buffer b ) 
            {
                // TODO: parse & split messages (from "(n)(m)" to "(n)", "(m)")
                std::string::size_type lf = b.find('\n');
                unfinished_buffer[clientid] += b;
                if(lf == std::string::npos)
                {
                    std::cerr << "ClientEventListener::newData: Line not finished; buffering\n";
                    return;
                }

                std::vector<std::string> read_strings;
                splitstr_and_fill(read_strings, unfinished_buffer[clientid], "\n");
                unfinished_buffer[clientid] = read_strings.back();
                read_strings.pop_back();
                BOOST_FOREACH(std::string s, read_strings)
                {
                    handleMessage(clientid, s);
                }
            }

            void ClientEventListener::handleMessage(int clientid, buffer b)
            {
                using namespace messages;
                if(b.length() == 0) return;

                std::string t;
                try
                {
                    t = getMessageType(b);
                }
                catch(...)
                {
                    std::cerr << "ClientEventListener: received invalid message: " << b << std::endl;
                    return;
                }

                ClientList::iterator it = mClientList->find(clientid);
                if(it == mClientList->end())
                {
                    std::cerr << "ClientEventListener::handleMessage: client not in the client list? (Should never happen.)\n";
                    return;
                }

                // MovePlayerControlMessage: handled by InputMonitor.
                if (t == c_pl_ctl_move)
                {
                    handleMoveMessage(clientid, b, it->second);
                    return;
                }

                // KickPlayerControlMessage: handled by InputMonitor.
                else if (t == c_pl_ctl_kick)
                {
                    handleKickMessage(clientid, b, it->second);
                    return;
                }

                // HoldPlayerControlMessage: handled by InputMonitor.
                else if (t == c_pl_ctl_hold)
                {
                    handleHoldMessage(clientid, b, it->second);
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

                        std::set<int> ps = m.getList();

                        std::set<int> clients_that_need_player_list_update;

                        ClientType this_controller = (it->second->getAI()) ? AIClient : HumanClient;

                        std::set<int>::iterator plidit;
                        for(plidit = ps.begin(); plidit != ps.end(); plidit++)
                        {
                            try
                            {
                                boost::tuple<ClientType, boost::shared_ptr<Client> > prev_controller = getControllerType(mClientList, *plidit);
                                if(prev_controller.get<1>()->getID() == clientid)
                                    continue;
                                if(prev_controller.get<0>() == HumanClient || (prev_controller.get<0>() == AIClient && this_controller != HumanClient))
                                {   // taking over not allowed
                                    std::cout << "Client " << clientid << " wants to take over player " << *plidit << "; denied.\n";
                                    ps.erase(plidit);
                                    plidit = ps.begin();
                                }
                                else if(prev_controller.get<0>() == AIClient)
                                {   // human takes over ai -> ai gets kicked out
                                    std::cout << "Client " << clientid << " wants to take over player " << *plidit << "; approved.\n";
                                    prev_controller.get<1>()->removePlayer(*plidit);
                                    clients_that_need_player_list_update.insert(prev_controller.get<1>()->getID());
                                }
                            }
                            catch(...)
                            {
                                continue;
                            }
                        }

                        (*it).second->clearPlayers();
                        (*it).second->addPlayers(ps);

                        if(clients_that_need_player_list_update.size() > 0)
                            clients_that_need_player_list_update.insert(clientid);

                        BOOST_FOREACH(int updid, clients_that_need_player_list_update)
                        {
                            mDispatcher->sendPlayerList(updid);
                        }
                    }
                    catch(const char* c)
                    {
                        std::cerr << "ClientEventListener: failed to parse PlayerControlRequestMessage: " << c << "\n";
                    }
                    catch(...)
                    {
                        std::cerr << "ClientEventListener: failed to parse PlayerControlRequestMessage.\n";
                    }
                    return;
                }

                // Set General Update Interval: Dispatcher
                else if (t == c_set_gen_upd_int)
                {
                    try
                    {
                        const messages::SetGeneralUpdateIntervalMessage m(b);
                        mDispatcher->newClientMessage(clientid, m);
                    }
                    catch(...)
                    {
                        std::cerr << "ClientEventListener: failed to parse SetGeneralUpdateIntervalMessage.\n";
                    }
                    return;
                }

                // Get General Update Interval: Dispatcher
                else if (t == c_get_gen_upd_int)
                {
                    const messages::GetGeneralUpdateInterval m;
                    mDispatcher->newClientMessage(clientid, m);
                    return;
                }

                // Set Constant Update Interval: Dispatcher
                else if (t == c_set_const_upd_int)
                {
                    try
                    {
                        const messages::SetConstantUpdateIntervalMessage m(b);
                        mDispatcher->newClientMessage(clientid, m);
                    }
                    catch(...)
                    {
                        std::cerr << "ClientEventListener: failed to parse SetConstantUpdateIntervalMessage.\n";
                    }
                    return;
                }

                // Get Constant Update Interval: Dispatcher
                else if (t == c_get_const_upd_int)
                {
                    const messages::GetConstantUpdateInterval m;
                    mDispatcher->newClientMessage(clientid, m);
                    return;
                }

                // TODO: add handling for all messages
            }

            void ClientEventListener::handleMoveMessage(int clientid, buffer& b, boost::shared_ptr<Client> c)
            {
                try
                {
                    const messages::MovePlayerControlMessage m(b);
                    int playerid = m.getPlayerID();
                    if(checkController(clientid, playerid, c))
                        mInputMonitor->newClientMessage(m);
                }
                catch(...)
                {
                    std::cerr << "ClientEventListener: failed to parse PlayerControlMessage.\n";
                }
                return;
            }

            // TODO: merge this with MovePlayerControlMessage for nicer code?
            void ClientEventListener::handleKickMessage(int clientid, buffer& b, boost::shared_ptr<Client> c)
            {
                try
                {
                    const messages::KickPlayerControlMessage m(b);
                    int playerid = m.getPlayerID();
                    if(checkController(clientid, playerid, c))
                        mInputMonitor->newClientMessage(m);
                }
                catch(...)
                {
                    std::cerr << "ClientEventListener: failed to parse PlayerControlMessage.\n";
                }
                return;
            }

            void ClientEventListener::handleHoldMessage(int clientid, buffer& b, boost::shared_ptr<Client> c)
            {
                try
                {
                    const messages::HoldPlayerControlMessage m(b);
                    int playerid = m.getPlayerID();
                    if(checkController(clientid, playerid, c))
                        mInputMonitor->newClientMessage(m);
                }
                catch(...)
                {
                    std::cerr << "ClientEventListener: failed to parse PlayerControlMessage.\n";
                }
                return;
            }

            bool ClientEventListener::checkController(int clientid, int playerid, boost::shared_ptr<Client> c)
            {
                if(!c->controlsPlayer(playerid))
                {
                    std::cerr << "ClientEventListener: client " << clientid << " trying to control another player (" << playerid << ").\n";
                    if(c->getAI())
                    {
                        mDispatcher->sendPlayerList(clientid);
                    }
                    return false;
                }
                return true;
            }
        }
    }
}

