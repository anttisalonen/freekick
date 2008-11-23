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

#include "ConnectionListener.h"

// Constructors/Destructors
//  

namespace freekick
{
    namespace match
    {
        namespace server
        {
            ConnectionListener::ConnectionListener ( ) {
                initAttributes();
            }

            ConnectionListener::~ConnectionListener ( ) { }

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
            void ConnectionListener::setMIoService ( io_service new_var ) {
                mIoService = new_var;
            }

/**
 * Get the value of mIoService
 * @return the value of mIoService
 */
            io_service ConnectionListener::getMIoService ( ) {
                return mIoService;
            }

/**
 * Set the value of mClientList
 * @param new_var the new value of mClientList
 */
            void ConnectionListener::setMClientList ( freekick::match::ClientList new_var ) {
                mClientList = new_var;
            }

/**
 * Get the value of mClientList
 * @return the value of mClientList
 */
            freekick::match::ClientList ConnectionListener::getMClientList ( ) {
                return mClientList;
            }

/**
 * Set the value of mDispatcher
 * @param new_var the new value of mDispatcher
 */
            void ConnectionListener::setMDispatcher ( freekick::match::server::Dispatcher new_var ) {
                mDispatcher = new_var;
            }

/**
 * Get the value of mDispatcher
 * @return the value of mDispatcher
 */
            freekick::match::server::Dispatcher ConnectionListener::getMDispatcher ( ) {
                return mDispatcher;
            }

// Other methods
//  


/**
 * @param  c
 */
            void ConnectionListener::newClient (freekick::match::Client c ) {

            }

            void ConnectionListener::initAttributes ( ) {
            }
        }
    }
}

