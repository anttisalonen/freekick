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

#include "Physics.h"

// Constructors/Destructors
//  

namespace freekick
{
    namespace match
    {
        namespace server
        {
            Physics::Physics ( ) {
                initAttributes();
            }

            Physics::~Physics ( ) { }

//  
// Methods
//  


// Accessor methods
//  


// Private attribute accessor methods
//  


/**
 * Set the value of mPause
 * @param new_var the new value of mPause
 */
            void Physics::setMPause ( bool new_var ) {
                mPause = new_var;
            }

/**
 * Get the value of mPause
 * @return the value of mPause
 */
            bool Physics::getMPause ( ) {
                return mPause;
            }

/**
 * Set the value of mNewPhysicsEvents
 * @param new_var the new value of mNewPhysicsEvents
 */
            void Physics::setMNewPhysicsEvents ( freekick::match::PhysicsEventList new_var ) {
                mNewPhysicsEvents = new_var;
            }

/**
 * Get the value of mNewPhysicsEvents
 * @return the value of mNewPhysicsEvents
 */
            freekick::match::PhysicsEventList Physics::getMNewPhysicsEvents ( ) {
                return mNewPhysicsEvents;
            }

/**
 * Set the value of mPhysicsEngine
 * @param new_var the new value of mPhysicsEngine
 */
            void Physics::setMPhysicsEngine ( freekick::match::PhysicsEngine new_var ) {
                mPhysicsEngine = new_var;
            }

/**
 * Get the value of mPhysicsEngine
 * @return the value of mPhysicsEngine
 */
            freekick::match::PhysicsEngine Physics::getMPhysicsEngine ( ) {
                return mPhysicsEngine;
            }

/**
 * Set the value of mPhysicsState
 * @param new_var the new value of mPhysicsState
 */
            void Physics::setMPhysicsState ( freekick::match::PhysicsState new_var ) {
                mPhysicsState = new_var;
            }

/**
 * Get the value of mPhysicsState
 * @return the value of mPhysicsState
 */
            freekick::match::PhysicsState Physics::getMPhysicsState ( ) {
                return mPhysicsState;
            }

/**
 * Set the value of mDispatcher
 * @param new_var the new value of mDispatcher
 */
            void Physics::setMDispatcher ( freekick::match::server::Dispatcher new_var ) {
                mDispatcher = new_var;
            }

/**
 * Get the value of mDispatcher
 * @return the value of mDispatcher
 */
            freekick::match::server::Dispatcher Physics::getMDispatcher ( ) {
                return mDispatcher;
            }

// Other methods
//  


/**
 * @param  p
 */
            void Physics::setPause (bool p ) {

            }


/**
 */
            void Physics::run ( ) {

            }


/**
 * @param  e
 */
            void Physics::newClientEvent (freekick::match::ClientEvent e ) {

            }


/**
 * @return bool
 */
            bool Physics::stepPhysics ( ) {

            }

            void Physics::initAttributes ( ) {
                mPause = true;
            }
        }
    }
}

