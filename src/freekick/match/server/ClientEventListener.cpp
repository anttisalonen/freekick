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
            ClientEventListener::ClientEventListener ( ) {
                initAttributes();
            }

            ClientEventListener::~ClientEventListener ( ) { }

//  
// Methods
//  


// Accessor methods
//  


// Private attribute accessor methods
//  


/**
 * Set the value of mIoService
 * @param new_var the new value of mIoService
 */
            void ClientEventListener::setMIoService ( io_service new_var ) {
                mIoService = new_var;
            }

/**
 * Get the value of mIoService
 * @return the value of mIoService
 */
            io_service ClientEventListener::getMIoService ( ) {
                return mIoService;
            }

/**
 * Set the value of mClientList
 * @param new_var the new value of mClientList
 */
            void ClientEventListener::setMClientList ( freekick::match::ClientList new_var ) {
                mClientList = new_var;
            }

/**
 * Get the value of mClientList
 * @return the value of mClientList
 */
            freekick::match::ClientList ClientEventListener::getMClientList ( ) {
                return mClientList;
            }

// Other methods
//  


/**
 * @param  b
 */
            void ClientEventListener::newData (Buffer b ) {

            }

            void ClientEventListener::initAttributes ( ) {
            }
        }
    }
}

