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


#ifndef FREEKICK_MATCH_CLIENT_H
#define FREEKICK_MATCH_CLIENT_H

#include <map>
#include <set>
#include <string>

#include <boost/foreach.hpp>
#include <boost/shared_ptr.hpp>
#include <boost/tuple/tuple.hpp>

namespace freekick 
{
    namespace match 
    {
        typedef int ClientID;
        typedef int PlayerID;

        enum ClientType
        {
            NoClient,
            HumanClient,
            AIClient
        };

        class Client
        {
        public:
            Client(ClientID clid, 
                   bool ai, 
                   const std::set<PlayerID>& cpl = std::set<PlayerID>(), 
                   const std::string& n = "", 
                   int const_upd_msg_int = 50, 
                   int gen_upd_msg_int = 1000);
            virtual ~Client ( );

            ClientID getID() const;
            void addPlayer(PlayerID pl);
            void addPlayers(const std::set<PlayerID>& pls);
            void removePlayer(PlayerID pl);
            void clearPlayers();
            const boost::shared_ptr<std::set<PlayerID> > getControlledPlayers() const;
            void setAI(bool a);
            bool getAI() const;
            const std::string& getCallsign() const;
            void setCallsign(const std::string& s);
            bool controlsPlayer(PlayerID pl) const;
            int getConstantUpdateMessageInterval() const;
            int getGeneralUpdateMessageInterval() const;
            int setConstantUpdateMessageInterval(int i);
            int setGeneralUpdateMessageInterval(int i);

        private:
            ClientID clientid;
            bool ai_controlled;
            std::string callsign;
            boost::shared_ptr<std::set<PlayerID> > controlled_players;
            int constant_update_message_interval;
            int general_update_message_interval;
        };

        typedef std::map<ClientID, boost::shared_ptr<freekick::match::Client> > ClientList;
        typedef boost::shared_ptr<ClientList> ClientListPtr;

        boost::tuple<ClientType, boost::shared_ptr<freekick::match::Client> > getControllerType(const ClientListPtr& list, PlayerID pl);
    }
}

#endif // FREEKICK_MATCH_CLIENT_H
