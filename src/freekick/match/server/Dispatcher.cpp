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

#include "Dispatcher.h"

// Constructors/Destructors
//  

namespace freekick
{
    namespace match
    {
        namespace server
        {
            Dispatcher::Dispatcher ( ) {
                initAttributes();
            }

            Dispatcher::~Dispatcher ( ) { }

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
            void Dispatcher::setMIoService ( io_service new_var ) {
                m_mIoService = new_var;
            }

/**
 * Get the value of m_mIoService
 * @return the value of m_mIoService
 */
            io_service Dispatcher::getMIoService ( ) {
                return m_mIoService;
            }

/**
 * Set the value of m_mClientList
 * @param new_var the new value of m_mClientList
 */
            void Dispatcher::setMClientList ( freekick::match::ClientList new_var ) {
                m_mClientList = new_var;
            }

/**
 * Get the value of m_mClientList
 * @return the value of m_mClientList
 */
            freekick::match::ClientList Dispatcher::getMClientList ( ) {
                return m_mClientList;
            }

// Other methods
//  


/**
 * @param  ioservice
 */
            Dispatcher::Dispatcher (io_service ioservice ) {

            }


/**
 * @param  e
 */
            void Dispatcher::dispatchPhysicsEvent (freekick::match::PhysicsEvent e ) {

            }


/**
 * @param  e
 */
            void Dispatcher::dispatchRulesEvent (freekick::match::RulesEvent e ) {

            }


/**
 * @param  s
 */
            void Dispatcher::dispatchPhysicsState (freekick::match::PhysicsState s ) {

            }


/**
 * @param  s
 */
            void Dispatcher::dispatchRulesState (freekick::match::RulesState s ) {

            }


/**
 */
            void Dispatcher::dispatchClientInformation ( ) {

            }


/**
 * @param  e
 */
            void Dispatcher::dispatchConnectionEvent (freekick::match::ConnectionEvent e ) {

            }


/**
 * @param  es
 */
            void Dispatcher::dispatchPhysicsEvents (freekick::match::PhysicsEventList es ) {

            }


/**
 * @param  es
 */
            void Dispatcher::dispatchRulesEvents (freekick::match::RulesEventList es ) {

            }


/**
 * @param  es
 */
            void Dispatcher::dispatchConnectionEvents (freekick::match::ConnectionEventList es ) {

            }

            void Dispatcher::initAttributes ( ) {
            }
        }
    }
}

