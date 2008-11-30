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


#ifndef FREEKICK_MATCH_CLIENT_H
#define FREEKICK_MATCH_CLIENT_H

#include <map>
#include <set>
#include <string>

#include <boost/shared_ptr.hpp>

namespace freekick 
{
    namespace match 
    {

        class Client
        {
        public:
            Client(unsigned int clid = 0, std::string n = "", bool ai = false);
            Client(unsigned int clid, unsigned int pl, std::string n = "", bool ai = false);
            Client(unsigned int clid, std::set<unsigned int> cpl, std::string n = "", bool ai = false);
            virtual ~Client ( );

            unsigned int getID() const;
            void addPlayer(unsigned int pl);
            void setAI(bool a);
            bool getAI() const;
            const std::string& getCallsign() const;
            void setCallsign(const std::string& s);
            bool controlsPlayer(unsigned int pl) const;

        private:
            unsigned int clientid;
            std::string callsign;
            bool ai_controlled;
            std::set<unsigned int> controlled_players;
        };
        typedef std::map<unsigned int, freekick::match::Client> ClientList;
        typedef boost::shared_ptr<ClientList> ClientListPtr;
    }
}

#endif // FREEKICK_MATCH_CLIENT_H
