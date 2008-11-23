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
 * Set the value of m_mPause
 * @param new_var the new value of m_mPause
 */
            void Physics::setMPause ( bool new_var ) {
                m_mPause = new_var;
            }

/**
 * Get the value of m_mPause
 * @return the value of m_mPause
 */
            bool Physics::getMPause ( ) {
                return m_mPause;
            }

/**
 * Set the value of m_mNewPhysicsEvents
 * @param new_var the new value of m_mNewPhysicsEvents
 */
            void Physics::setMNewPhysicsEvents ( freekick::match::PhysicsEventList new_var ) {
                m_mNewPhysicsEvents = new_var;
            }

/**
 * Get the value of m_mNewPhysicsEvents
 * @return the value of m_mNewPhysicsEvents
 */
            freekick::match::PhysicsEventList Physics::getMNewPhysicsEvents ( ) {
                return m_mNewPhysicsEvents;
            }

/**
 * Set the value of m_mPhysicsEngine
 * @param new_var the new value of m_mPhysicsEngine
 */
            void Physics::setMPhysicsEngine ( freekick::match::PhysicsEngine new_var ) {
                m_mPhysicsEngine = new_var;
            }

/**
 * Get the value of m_mPhysicsEngine
 * @return the value of m_mPhysicsEngine
 */
            freekick::match::PhysicsEngine Physics::getMPhysicsEngine ( ) {
                return m_mPhysicsEngine;
            }

/**
 * Set the value of m_mPhysicsState
 * @param new_var the new value of m_mPhysicsState
 */
            void Physics::setMPhysicsState ( freekick::match::PhysicsState new_var ) {
                m_mPhysicsState = new_var;
            }

/**
 * Get the value of m_mPhysicsState
 * @return the value of m_mPhysicsState
 */
            freekick::match::PhysicsState Physics::getMPhysicsState ( ) {
                return m_mPhysicsState;
            }

/**
 * Set the value of m_mDispatcher
 * @param new_var the new value of m_mDispatcher
 */
            void Physics::setMDispatcher ( freekick::match::server::Dispatcher new_var ) {
                m_mDispatcher = new_var;
            }

/**
 * Get the value of m_mDispatcher
 * @return the value of m_mDispatcher
 */
            freekick::match::server::Dispatcher Physics::getMDispatcher ( ) {
                return m_mDispatcher;
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
                m_mPause = true;
            }
        }
    }
}

