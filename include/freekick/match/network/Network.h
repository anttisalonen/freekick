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
#include <boost/lexical_cast.hpp>
#include <boost/algorithm/string/find.hpp>
#include <boost/algorithm/string/split.hpp>
#include <boost/algorithm/string/find_iterator.hpp>
#include <boost/algorithm/string/classification.hpp>
#include <boost/foreach.hpp>
#include <boost/date_time/posix_time/posix_time.hpp>

#include "MatchStatus.h"
#include "IP_Connection.h"
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
            class Network
            {
            public:

                /**
                 * @param  conf
                 * @param  stat
                 */
                Network (addutil::network::IP_Connection conn, MatchStatus* stat );
                virtual ~Network();

                /**
                 * @return bool
                 * @param  data
                 */
                bool sendData (const std::string* data );

                /**
                 */
                bool run ( );

            protected:

                /**
                 * @return string*
                 */
                std::string* readData ( );

            private:
                void connect();
                void handle_resolve(const boost::system::error_code& err, 
                                    boost::asio::ip::tcp::resolver::iterator endpoint_iterator);
                void handle_connect(const boost::system::error_code& err, 
                                    boost::asio::ip::tcp::resolver::iterator endpoint_iterator);
                void handle_write_data(const boost::system::error_code& err);
                void handle_read_handshake(const boost::system::error_code& err);
                void handle_read_events(const boost::system::error_code& err, std::size_t bytes);
                // Private attributes
                //  

                freekick::match::MatchStatus* status;
                addutil::network::IP_Connection ip_conn;
                boost::asio::io_service io_service;
                boost::asio::ip::tcp::resolver resolver;
                boost::asio::streambuf serverdata;
                boost::asio::streambuf senddata;
                boost::asio::ip::tcp::socket serversocket;
            };
        }
    }
}

#endif // NETWORK_H
