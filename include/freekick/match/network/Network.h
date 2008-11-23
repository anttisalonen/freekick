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
  This file was generated on So Okt 26 2008 at 12:09:20
**************************************************************************/


#ifndef NETWORK_H
#define NETWORK_H

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

#include "MatchStatus.h"
#include "addutil/Parsing.h"
#include "addutil/network/Client.h"
#include "addutil/network/IP_Connection.h"
#include "Event.h"

namespace freekick
{
    namespace match
    {
        namespace network
        {
            class NetworkException : public boost::exception
            {
                NetworkException(const std::string& s) { }
            };
        }
    }
}
/**
 * class Network
 */

namespace freekick
{
    namespace match
    {
        namespace network
        {
            class Network : public addutil::network::Client
            {
            public:

                /**
                 * @param  conf
                 * @param  stat
                 */
                Network (addutil::network::IP_Connection conn, MatchStatus* stat );
                virtual ~Network();

                bool run ( );

            protected:
                void read(std::string buf);

            private:
                // Private attributes
                //  

                freekick::match::MatchStatus* status;
                std::string buffer;
                bool handshake;
            };
        }
    }
}

#endif // NETWORK_H
