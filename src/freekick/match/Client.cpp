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

#include "freekick/match/Client.h"

// Constructors/Destructors
//  

namespace freekick
{
    namespace match
    {
        Client::Client(ClientID clid, bool ai, const std::set<PlayerID>& cpl, const std::string& n, int const_upd_msg_int, int gen_upd_msg_int)
            : clientid(clid),
              ai_controlled(ai),
              callsign(n),
              controlled_players(new std::set<PlayerID> (cpl)),
              constant_update_message_interval(const_upd_msg_int),
              general_update_message_interval(gen_upd_msg_int)
        {
        }

        Client::~Client ( ) 
        { 
        }

        ClientID Client::getID() const
        {
            return clientid;
        }

        void Client::setAI(bool a)
        {
            ai_controlled = a;
        }

        void Client::setCallsign(const std::string& s)
        {
            callsign = s;
        }
            
        bool Client::getAI() const
        {
            return ai_controlled;
        }

        const std::string& Client::getCallsign() const
        {
            return callsign;
        }

        void Client::addPlayer(PlayerID pl)
        {
            controlled_players->insert(pl);
        }

        void Client::addPlayers(const std::set<PlayerID>& pls)
        {
            BOOST_FOREACH(PlayerID pl, pls)
            {
                controlled_players->insert(pl);
            }
        }

        void Client::clearPlayers()
        {
            controlled_players->clear();
        }

        void Client::removePlayer(PlayerID pl)
        {
            controlled_players->erase(pl);
        }

        const boost::shared_ptr<std::set<PlayerID> > Client::getControlledPlayers() const
        {
            return controlled_players;
        }

        bool Client::controlsPlayer(PlayerID pl) const
        {
            return (controlled_players->find(pl) != controlled_players->end());
        }

        int Client::getConstantUpdateMessageInterval() const
        {
            return constant_update_message_interval;
        }

        int Client::getGeneralUpdateMessageInterval() const
        {
            return general_update_message_interval;
        }

        int Client::setConstantUpdateMessageInterval(int i)
        {
            constant_update_message_interval = i;
            return constant_update_message_interval;
        }

        int Client::setGeneralUpdateMessageInterval(int i)
        {
            general_update_message_interval = i;
            return general_update_message_interval;
        }

        boost::tuple<ClientType, boost::shared_ptr<freekick::match::Client> > getControllerType(const ClientListPtr& list, PlayerID pl)
        {
            ClientList::const_iterator it;
            for(it = list->begin(); it != list->end(); it++)
            {
                if(it->second->controlsPlayer(pl))
                {
                    ClientType t;
                    if(it->second->getAI()) t = AIClient;
                    else t = HumanClient;
                    return boost::tuple<ClientType, boost::shared_ptr<freekick::match::Client> >(t, it->second);
                }
            }
            throw "getControllerType: no client found.\n";
        }
    }
}

