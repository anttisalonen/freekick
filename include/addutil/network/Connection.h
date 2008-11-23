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
#include "IP_Connection.h"

/**
 * class Connection
 */

namespace addutil
{
    namespace network
    {
        typedef std::vector<char> msgbuffer;
        typedef long conn_id;
        class Connection
        {
        public:
            Connection(conn_id _id);
            virtual ~Connection() { if(connected()) disconnect(); }
            void read(msgbuffer& buf);
            void write(const msgbuffer& buf);
            bool connect(const IP_Connection& tgt);
            void disconnect();
            bool connected();
        private:
            conn_id id;
            IP_Connection conn;
        };
    }
}

#endif // ADDUTIL_NETWORK_CONNECTION_H
