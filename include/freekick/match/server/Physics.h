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


#ifndef PHYSICS_H
#define PHYSICS_H





namespace freekick
{
    namespace match
    {
        namespace server
        {
            class Physics
            {
            public:

                // Constructors/Destructors
                //  


                /**
                 * Empty Constructor
                 */
                Physics ( );

                /**
                 * Empty Destructor
                 */
                virtual ~Physics ( );



                /**
                 * @param  p
                 */
                void setPause (bool p );


                /**
                 */
                void run ( );


                /**
                 * @param  e
                 */
                void newClientEvent (freekick::match::ClientEvent e );


                /**
                 * @return bool
                 */
                bool stepPhysics ( );

            protected:

            public:

            protected:

            public:

            protected:


            private:

                // Private attributes
                //  

                bool m_mPause;
                freekick::match::PhysicsEventList m_mNewPhysicsEvents;
                freekick::match::PhysicsEngine m_mPhysicsEngine;
                freekick::match::PhysicsState m_mPhysicsState;
                freekick::match::server::Dispatcher m_mDispatcher;
            public:

            private:

            public:


                // Private attribute accessor methods
                //  


                /**
                 * Set the value of m_mPause
                 * @param new_var the new value of m_mPause
                 */
                void setMPause ( bool new_var );

                /**
                 * Get the value of m_mPause
                 * @return the value of m_mPause
                 */
                bool getMPause ( );


                /**
                 * Set the value of m_mNewPhysicsEvents
                 * @param new_var the new value of m_mNewPhysicsEvents
                 */
                void setMNewPhysicsEvents ( freekick::match::PhysicsEventList new_var );

                /**
                 * Get the value of m_mNewPhysicsEvents
                 * @return the value of m_mNewPhysicsEvents
                 */
                freekick::match::PhysicsEventList getMNewPhysicsEvents ( );


                /**
                 * Set the value of m_mPhysicsEngine
                 * @param new_var the new value of m_mPhysicsEngine
                 */
                void setMPhysicsEngine ( freekick::match::PhysicsEngine new_var );

                /**
                 * Get the value of m_mPhysicsEngine
                 * @return the value of m_mPhysicsEngine
                 */
                freekick::match::PhysicsEngine getMPhysicsEngine ( );


                /**
                 * Set the value of m_mPhysicsState
                 * @param new_var the new value of m_mPhysicsState
                 */
                void setMPhysicsState ( freekick::match::PhysicsState new_var );

                /**
                 * Get the value of m_mPhysicsState
                 * @return the value of m_mPhysicsState
                 */
                freekick::match::PhysicsState getMPhysicsState ( );


                /**
                 * Set the value of m_mDispatcher
                 * @param new_var the new value of m_mDispatcher
                 */
                void setMDispatcher ( freekick::match::server::Dispatcher new_var );

                /**
                 * Get the value of m_mDispatcher
                 * @return the value of m_mDispatcher
                 */
                freekick::match::server::Dispatcher getMDispatcher ( );

            private:


                void initAttributes ( ) ;

            };
        }
    }
}

#endif // PHYSICS_H
