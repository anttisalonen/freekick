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

#include "Client.h"

namespace addutil
{
    namespace network
    {
        Client::Client (IP_Connection conn)
            : ip_conn(conn), 
              c(1, ioserv)
        {
        }
            
        Client::~Client()
        {
            try
            {
                disconnect();
            }
            catch(...)
            {
                // ignore?
            }
        }

        void Client::setTarget(const IP_Connection& conn)
        {
            ip_conn = conn;
        }

        void Client::getTarget(IP_Connection& conn) const
        {
            conn = ip_conn;
        }

        void Client::connect()
        {
            disconnect();
            c.connect(ip_conn);
            read_loop();
        }

        void Client::disconnect()
        {
            if(c.connected())
                c.disconnect();
        }

        bool Client::is_connected()
        {
            return c.connected();
        }

        void Client::read_loop()
        {
            while(c.connected())
            {
                boost::shared_ptr<std::string> n;
                c.read(n);
                read(*n);
            }
        }

        void Client::write(const std::string& buf)
        {
            if(c.connected())
                c.write(buf);
            else throw "Client::write: not connected";
        }
    }
}
