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
 * Set the value of mContinue
 * @param new_var the new value of mContinue
 */
            void Console::setMContinue ( bool new_var ) {
                mContinue = new_var;
            }

/**
 * Get the value of mContinue
 * @return the value of mContinue
 */
            bool Console::getMContinue ( ) {
                return mContinue;
            }

/**
 * Set the value of mPhysics
 * @param new_var the new value of mPhysics
 */
            void Console::setMPhysics ( boost::shared_ptr<Physics> new_var ) {
                mPhysics = new_var;
            }

/**
 * Get the value of mPhysics
 * @return the value of mPhysics
 */
            boost::shared_ptr<Physics> Console::getMPhysics ( ) {
                return mPhysics;
            }

/**
 * Set the value of mRules
 * @param new_var the new value of mRules
 */
            void Console::setMRules ( boost::shared_ptr<Rules> new_var ) {
                mRules = new_var;
            }

/**
 * Get the value of mRules
 * @return the value of mRules
 */
            boost::shared_ptr<Rules> Console::getMRules ( ) {
                return mRules;
            }

/**
 * Set the value of mConnectionListener
 * @param new_var the new value of mConnectionListener
 */
            void Console::setMConnectionListener ( boost::shared_ptr<ConnectionListener> new_var ) {
                mConnectionListener = new_var;
            }

/**
 * Get the value of mConnectionListener
 * @return the value of mConnectionListener
 */
            boost::shared_ptr<ConnectionListener> Console::getMConnectionListener ( ) {
                return mConnectionListener;
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
                mContinue = true;
            }
        }
    }
}

