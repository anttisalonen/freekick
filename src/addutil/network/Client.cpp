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

#include "Client.h"

namespace addutil
{
    namespace network
    {
        Client::Client (IP_Connection conn)
            : ip_conn(conn), 
              ioserv(new boost::asio::io_service()),
              resolver(*ioserv),
              serversocket(*ioserv)
        {
        }
            
        Client::~Client()
        {
            serversocket.close();
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
            std::cerr << "Connecting...\n";
            using boost::asio::ip::tcp;
            // Get a list of endpoints corresponding to the server name.
            tcp::resolver::query query(ip_conn.ip_address, ip_conn.port);
            tcp::resolver::iterator endpoint_iterator = resolver.resolve(query);
            tcp::resolver::iterator end;

            // Try each endpoint until we successfully establish a connection.
            boost::system::error_code error = boost::asio::error::host_not_found;
            while (error && endpoint_iterator != end)
            {
                serversocket.close();
                serversocket.connect(*endpoint_iterator++, error);
            }
            if (error)
                throw boost::system::system_error(error);

            std::cerr << "Connected!\n";
            read_loop();
        }

        void Client::disconnect()
        {
            serversocket.close();
        }

        void Client::read_loop()
        {
            boost::asio::streambuf indata;
            boost::system::error_code error;
            while (boost::asio::read(serversocket, indata,
                                     boost::asio::transfer_at_least(1), error))
            {
                std::istream datastream(&indata);
                boost::asio::streambuf::const_buffers_type bufs = indata.data();
                size_t bytes = indata.size();
                std::string line(boost::asio::buffers_begin(bufs),
                                 boost::asio::buffers_begin(bufs) + bytes);
                indata.consume(bytes);
                read(line);
            }
        }

        void Client::write(const std::string& buf)
        {
            boost::asio::streambuf resp_stream;
            std::ostream wr_stream(&resp_stream);
            wr_stream << buf;
            boost::asio::write(serversocket, resp_stream);
        }
    }
}
