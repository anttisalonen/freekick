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

#include "Console.h"

// Constructors/Destructors
//  

namespace freekick
{
    namespace match
    {
        namespace server
        {
            Console::Console ( ) {
                initAttributes();
            }

            Console::~Console ( ) { }

//  
// Methods
//  


// Accessor methods
//  


// Private attribute accessor methods
//  


/**
 * Set the value of m_mContinue
 * @param new_var the new value of m_mContinue
 */
            void Console::setMContinue ( bool new_var ) {
                m_mContinue = new_var;
            }

/**
 * Get the value of m_mContinue
 * @return the value of m_mContinue
 */
            bool Console::getMContinue ( ) {
                return m_mContinue;
            }

/**
 * Set the value of m_mPhysics
 * @param new_var the new value of m_mPhysics
 */
            void Console::setMPhysics ( boost::shared_ptr<Physics> new_var ) {
                m_mPhysics = new_var;
            }

/**
 * Get the value of m_mPhysics
 * @return the value of m_mPhysics
 */
            boost::shared_ptr<Physics> Console::getMPhysics ( ) {
                return m_mPhysics;
            }

/**
 * Set the value of m_mRules
 * @param new_var the new value of m_mRules
 */
            void Console::setMRules ( boost::shared_ptr<Rules> new_var ) {
                m_mRules = new_var;
            }

/**
 * Get the value of m_mRules
 * @return the value of m_mRules
 */
            boost::shared_ptr<Rules> Console::getMRules ( ) {
                return m_mRules;
            }

/**
 * Set the value of m_mConnectionListener
 * @param new_var the new value of m_mConnectionListener
 */
            void Console::setMConnectionListener ( boost::shared_ptr<ConnectionListener> new_var ) {
                m_mConnectionListener = new_var;
            }

/**
 * Get the value of m_mConnectionListener
 * @return the value of m_mConnectionListener
 */
            boost::shared_ptr<ConnectionListener> Console::getMConnectionListener ( ) {
                return m_mConnectionListener;
            }

// Other methods
//  


/**
 * @param  physics
 * @param  rules
 * @param  connectionlistener
 */
            Console::Console (freekick::match::server::Physics physics, freekick::match::server::Rules rules, freekick::match::server::ConnectionListener connectionlistener ) {

            }


/**
 */
            void Console::run ( ) {

            }


/**
 * @param  cc
 */
            void Console::getInput (freekick::match::server::ConsoleCommand cc ) const {

            }

            void Console::initAttributes ( ) {
                m_mContinue = true;
            }
        }
    }
}

