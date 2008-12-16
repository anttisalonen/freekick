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
  This file was generated on Sa Nov 22 2008 at 11:40:55
**************************************************************************/

#include "ClientEventListener.h"

// Constructors/Destructors
//  

namespace freekick
{
    namespace match
    {
        namespace server
        {
            ClientEventListener::ClientEventListener (ClientListPtr clp, boost::shared_ptr<Physics> p)
                : mClientList(clp),
                  mPhysics(p)
            {
            }

            ClientEventListener::~ClientEventListener ( ) 
            { 
            }

            void ClientEventListener::newData (unsigned int id, buffer b ) 
            {
                using namespace messages;

                std::cerr << "ClientEventListener: New data received\n";
                // TODO: validate, process, pass on to physics
                std::string t;
                try
                {
                    t = getMessageType(b);
                }
                catch(...)
                {
                    std::cerr << "ClientEventListener: received invalid message.\n";
                    return;
                }
                if(t == c_pl_ctl_move)
                {
                    try
                    {
                        const messages::MovePlayerControlMessage m(b);
                        mPhysics->newClientMessage(m);
                    }
                    catch(...)
                    {
                        std::cerr << "ClientEventListener: failed to parse MovePlayerControlMessage.\n";
                        return;
                    }
                }
            }
/*
            void visit(const messages::MovePlayerControlMessage& mpcm)
            {
                mPhysics->newClientEvent(m);
            }
*/
        }
    }
}

