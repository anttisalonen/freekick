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
**************************************************************************/


#ifndef ADDUTIL_NETWORK_CONNECTION_H
#define ADDUTIL_NETWORK_CONNECTION_H

#include <vector>

#include <boost/asio.hpp>
#include <boost/shared_ptr.hpp>


#include "IP_Connection.h"

/**
 * class Connection
 */

namespace addutil
{
    namespace network
    {
        typedef std::string msgbuffer;
        typedef long conn_id;
        class Connection
        {
        public:
            Connection(boost::asio::io_service& ios, conn_id _id);
            virtual ~Connection();
            void read(boost::shared_ptr<msgbuffer>& b);
            void write(const msgbuffer& buf);
            void connect(const IP_Connection& tgt);
            void disconnect();
            bool connected();
            conn_id getID();
            IP_Connection& getConnection();
        private:
            boost::asio::io_service& ioserv;
            boost::asio::ip::tcp::socket socket;
            conn_id id;
            IP_Connection conn;
            bool mConnected;
        };
    }
}

#endif // ADDUTIL_NETWORK_CONNECTION_H
