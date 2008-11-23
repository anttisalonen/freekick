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
 * Set the value of m_mIoService
 * @param new_var the new value of m_mIoService
 */
            void ConnectionListener::setMIoService ( io_service new_var ) {
                m_mIoService = new_var;
            }

/**
 * Get the value of m_mIoService
 * @return the value of m_mIoService
 */
            io_service ConnectionListener::getMIoService ( ) {
                return m_mIoService;
            }

/**
 * Set the value of m_mClientList
 * @param new_var the new value of m_mClientList
 */
            void ConnectionListener::setMClientList ( freekick::match::ClientList new_var ) {
                m_mClientList = new_var;
            }

/**
 * Get the value of m_mClientList
 * @return the value of m_mClientList
 */
            freekick::match::ClientList ConnectionListener::getMClientList ( ) {
                return m_mClientList;
            }

/**
 * Set the value of m_mDispatcher
 * @param new_var the new value of m_mDispatcher
 */
            void ConnectionListener::setMDispatcher ( freekick::match::server::Dispatcher new_var ) {
                m_mDispatcher = new_var;
            }

/**
 * Get the value of m_mDispatcher
 * @return the value of m_mDispatcher
 */
            freekick::match::server::Dispatcher ConnectionListener::getMDispatcher ( ) {
                return m_mDispatcher;
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

