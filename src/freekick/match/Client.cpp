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

#include "freekick/match/Client.h"

// Constructors/Destructors
//  

namespace freekick
{
    namespace match
    {
        Client::Client (unsigned int clid, std::string n, bool ai) 
            : clientid(clid),
              callsign(n),
              ai_controlled(ai)
        {
        }

        Client::Client(unsigned int clid, unsigned int pl, std::string n, bool ai)
            : clientid(clid),
              callsign(n),
              ai_controlled(ai)
        {
            addPlayer(pl);
        }

        Client::Client(unsigned int clid, std::set<unsigned int> cpl, std::string n, bool ai)
            : clientid(clid),
              callsign(n),
              ai_controlled(ai),
              controlled_players(cpl)
        {
        }

        Client::~Client ( ) 
        { 
        }

        unsigned int Client::getID() const
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

        void Client::addPlayer(unsigned int pl)
        {
            controlled_players.insert(pl);
        }

        bool Client::controlsPlayer(unsigned int pl) const
        {
            return (controlled_players.find(pl) != controlled_players.end());
        }
    }
}

