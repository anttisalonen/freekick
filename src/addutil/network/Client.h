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
**************************************************************************/


#ifndef ADDUTIL_NETWORK_SERVER_H
#define ADDUTIL_NETWORK_SERVER_H

#include <boost/asio.hpp>
#include <boost/version.hpp>
#if BOOST_VERSION > 104000
#include <boost/exception/all.hpp>
#else
#include <boost/exception.hpp>
#endif
#include <boost/bind.hpp>
#include <boost/foreach.hpp>
#include <boost/shared_ptr.hpp>

#include "IP_Connection.h"
#include "Connection.h"

namespace addutil
{
    namespace network
    {
        class Client
        {
        public:
            Client(IP_Connection conn);
            virtual ~Client();
            virtual bool is_connected();
            void read_connection(bool nonblocking);
        protected:
            void connect(bool nonblocking = false);
            void disconnect();
            void setTarget(const IP_Connection& conn);
            void getTarget(IP_Connection& conn) const;
            virtual void read(std::string buf) = 0;
            void write(const std::string& buf);
        private:
            void read_loop();
            addutil::network::IP_Connection ip_conn;
            boost::asio::io_service ioserv;
            Connection c;
        };
    }
}

#endif // ADDUTIL_NETWORK_SERVER_H
