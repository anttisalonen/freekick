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
            }

            Physics::~Physics ( ) { }
/**
 * Set the value of mPause
 * @param new_var the new value of mPause
 */
            void Physics::setPause ( bool new_var ) {
                mPause = new_var;
            }

/**
 * Get the value of mPause
 * @return the value of mPause
 */
            bool Physics::getPause ( ) {
                return mPause;
            }

/**
 * Set the value of mNewPhysicsEvents
 * @param new_var the new value of mNewPhysicsEvents
 */
            void Physics::setNewPhysicsEvents ( PhysicsEventList new_var ) {
                mNewPhysicsEvents = new_var;
            }

/**
 * Get the value of mNewPhysicsEvents
 * @return the value of mNewPhysicsEvents
 */
            PhysicsEventList Physics::getNewPhysicsEvents ( ) {
                return mNewPhysicsEvents;
            }

/**
 * Set the value of mPhysicsEngine
 * @param new_var the new value of mPhysicsEngine
 */
            void Physics::setPhysicsEngine ( PhysicsEngine new_var ) {
                mPhysicsEngine = new_var;
            }

/**
 * Get the value of mPhysicsEngine
 * @return the value of mPhysicsEngine
 */
            PhysicsEngine Physics::getPhysicsEngine ( ) {
                return mPhysicsEngine;
            }

/**
 * Set the value of mPhysicsState
 * @param new_var the new value of mPhysicsState
 */
            void Physics::setPhysicsState ( PhysicsState new_var ) {
                mPhysicsState = new_var;
            }

/**
 * Get the value of mPhysicsState
 * @return the value of mPhysicsState
 */
            PhysicsState Physics::getPhysicsState ( ) {
                return mPhysicsState;
            }

// Other methods
//  

/**
 */
            void Physics::run ( ) {

            }


/**
 * @param  e
 */
            void Physics::newClientEvent (ClientEvent e ) {

            }


/**
 * @return bool
 */
            bool Physics::stepPhysics ( ) {
                return true;
            }
        }
    }
}

