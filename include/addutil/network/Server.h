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


#ifndef ADDUTIL_NETWORK_SERVER_H
#define ADDUTIL_NETWORK_SERVER_H

#include <vector>

#include <boost/asio.hpp>
#include <boost/exception.hpp>
#include <boost/bind.hpp>
#include <boost/foreach.hpp>
#include <boost/shared_ptr.hpp>
#include <boost/thread.hpp>

#include "Exception.h"
#include "IP_Connection.h"
#include "Connection.h"

namespace addutil
{
    namespace network
    {
        typedef unsigned int client_id;
        typedef boost::shared_ptr<Connection> ConnectionPtr;
        class Server
        {
        public:
            Server();
            virtual ~Server();
            void startListening(int port);
            void stopListening();
            virtual void client_connected(client_id id) = 0;
            virtual void client_disconnected(client_id id) = 0;
            void write(const std::string& msg, client_id id);
            void multicast(const std::string& msg, const std::vector<client_id>& ids);
            void broadcast(const std::string& msg);
            virtual void client_input(client_id id, const std::string& msg) = 0;

        private:
            void read_loop(ConnectionPtr c);
            void cleanup_client(client_id id);
            std::map<client_id, ConnectionPtr> connections;
            boost::shared_ptr<boost::asio::io_service> ioserv;
            client_id next_id;
            int portnumber;
            bool listening;
        };
    }
}

#endif // ADDUTIL_NETWORK_SERVER_H
