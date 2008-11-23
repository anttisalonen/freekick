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
 * Set the value of m_mIoService
 * @param new_var the new value of m_mIoService
 */
            void ClientEventListener::setMIoService ( io_service new_var ) {
                m_mIoService = new_var;
            }

/**
 * Get the value of m_mIoService
 * @return the value of m_mIoService
 */
            io_service ClientEventListener::getMIoService ( ) {
                return m_mIoService;
            }

/**
 * Set the value of m_mClientList
 * @param new_var the new value of m_mClientList
 */
            void ClientEventListener::setMClientList ( freekick::match::ClientList new_var ) {
                m_mClientList = new_var;
            }

/**
 * Get the value of m_mClientList
 * @return the value of m_mClientList
 */
            freekick::match::ClientList ClientEventListener::getMClientList ( ) {
                return m_mClientList;
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

