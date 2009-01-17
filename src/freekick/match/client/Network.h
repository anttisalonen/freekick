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
  This file was generated on So Okt 26 2008 at 12:09:20
**************************************************************************/


#ifndef FREEKICKCLIENTNETWORK_H
#define FREEKICKCLIENTNETWORK_H

#include <iostream>
#include <string>
#include <cstdlib>
#include <exception>
#include <vector>
#include <deque>
#include <cctype>

#include <boost/asio.hpp>
#include <boost/exception.hpp>
#include <boost/bind.hpp>
#include <boost/foreach.hpp>
#include <boost/date_time/posix_time/posix_time.hpp>
#include <boost/shared_ptr.hpp>

#include "addutil/Parsing.h"
#include "addutil/network/Client.h"
#include "addutil/network/IP_Connection.h"

#include "MatchStatus.h"
#include "Event.h"
#include "Lineup.h"
#include "MatchData.h"
#include "PlayerListHandler.h"

#include "messages/Messages.h"

namespace freekick
{
    namespace match
    {
        namespace client
        {
            class Network : public addutil::network::Client
            {
            public:
                Network (addutil::network::IP_Connection conn, bool ai);
                virtual ~Network();
                freekick::match::MatchStatus* getMatchStatus();

                bool run ( );
                void sendMessage(const messages::Message& m);
                void sendMessages(const std::vector<boost::shared_ptr<messages::Message> >& ms);
                void disconnect();
                int getConstantUpdateInterval() const;
                int getGeneralUpdateInterval() const;
                void setPlayerListHandler(const boost::shared_ptr<PlayerListHandler>& p);

            protected:
                void read(std::string buf);

            private:
                freekick::match::MatchStatus* status;
                std::string buffer;
                bool handshake;
                int constant_update_interval_ms;
                int general_update_interval_ms;
                bool aicontroller;
                boost::shared_ptr<PlayerListHandler> plh;
            };
        } 
    }
}

#endif // FREEKICKCLIENTNETWORK_H
