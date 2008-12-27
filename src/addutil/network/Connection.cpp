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

#include "Connection.h"

namespace addutil
{
    namespace network
    {
        Connection::Connection (conn_id _id, boost::asio::io_service& ios)
            : id(_id),
              ioserv(ios),
              socket(new boost::asio::ip::tcp::socket(ioserv)),
              mConnected(false)
        {
        }

        Connection::Connection(conn_id _id, boost::shared_ptr<boost::asio::ip::tcp::socket> s)
            : id(_id),
              ioserv(s->get_io_service()),
              socket(s),
              mConnected(true)
        {
            if(!connected()) throw "Connection::Connection: socket connection failed\n";
        }

        Connection::~Connection()
        {
            if(connected()) 
                disconnect();
        }

        void Connection::read(boost::shared_ptr<msgbuffer>& b)
        {
            boost::asio::streambuf indata;
            boost::system::error_code error;
            boost::asio::read(*socket, indata,
                              boost::asio::transfer_at_least(1), error);
            if(error) throw error;
            std::istream datastream(&indata);
            boost::asio::streambuf::const_buffers_type bufs = indata.data();
            size_t bytes = indata.size();
            std::string line(boost::asio::buffers_begin(bufs),
                              boost::asio::buffers_begin(bufs) + bytes);
            b.reset (new msgbuffer(line));
            indata.consume(bytes);
        }

        void Connection::write(const msgbuffer& buf)
        {
            if(!connected()) throw "Connection::write: not connected\n";
            boost::asio::streambuf resp_stream;
            std::ostream wr_stream(&resp_stream);
            wr_stream << buf;
            boost::asio::write(*socket, resp_stream);
        }

        void Connection::connect(const IP_Connection& tgt)
        {
            std::cerr << "Connection: Connecting...\n";
            conn = tgt;
            using boost::asio::ip::tcp;
            // Get a list of endpoints corresponding to the server name.
            tcp::resolver::query query(tgt.ip_address, tgt.port);
            boost::asio::ip::tcp::resolver resolver(ioserv);
            tcp::resolver::iterator endpoint_iterator = resolver.resolve(query);
            tcp::resolver::iterator end;

            // Try each endpoint until we successfully establish a connection.
            boost::system::error_code error = boost::asio::error::host_not_found;
            while (error && endpoint_iterator != end)
            {
                socket->close();
                socket->connect(*endpoint_iterator++, error);
            }
            if (error)
                throw boost::system::system_error(error);

            if(!connected()) throw "Connection::connect: Error while connecting";
            std::cerr << "Connection: Connected!\n";
        }

        void Connection::disconnect()
        {
            // std::cerr << "Connection::disconnect: disconnecting.\n";
            socket->close();
            if(connected()) throw "Connection::disconnect: disconnection failed\n";
        }

        bool Connection::connected()
        {
            mConnected = socket->is_open();
            return mConnected;
        }

        conn_id Connection::getID()
        {
            return id;
        }

        IP_Connection& Connection::getConnection()
        {
            return conn;
        }
    }
}
