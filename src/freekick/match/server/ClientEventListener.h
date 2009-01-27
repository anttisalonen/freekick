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
  This file was generated on Sa Nov 22 2008 at 11:40:55
**************************************************************************/


#ifndef CLIENTEVENTLISTENER_H
#define CLIENTEVENTLISTENER_H

#include <map>
#include <string>
#include <iostream>

#include <boost/shared_ptr.hpp>
#include <boost/tuple/tuple.hpp>

#include "addutil/network/Server.h"
#include "addutil/Parsing.h"

#include "freekick/match/Client.h"
#include "Rules.h"
#include "Dispatcher.h"
#include "InputMonitor.h"

#include "messages/Messages.h"

namespace freekick
{
    namespace match
    {
        namespace server
        {
            typedef std::string buffer;
            class ClientEventListener
            {
            public:
                ClientEventListener (ClientListPtr clp, boost::shared_ptr<InputMonitor> im, boost::shared_ptr<Dispatcher> d);
                virtual ~ClientEventListener ( );
                void newData (int clientid, buffer b );

            private:
                ClientListPtr mClientList;
                boost::shared_ptr<InputMonitor> mInputMonitor;
                boost::shared_ptr<Dispatcher> mDispatcher;
                std::map<int, std::string> unfinished_buffer;

                void handleMessage(int clientid, buffer b);
                void handleMoveMessage(int clientid, buffer& b, boost::shared_ptr<Client> c);
                void handleKickMessage(int clientid, buffer& b, boost::shared_ptr<Client> c);
                void handleHoldMessage(int clientid, buffer& b, boost::shared_ptr<Client> c);
                bool checkController(int clientid, int playerid, boost::shared_ptr<Client> c);
            };
        }
    }
}

#endif // CLIENTEVENTLISTENER_H
