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

                bool mPause;
                freekick::match::PhysicsEventList mNewPhysicsEvents;
                freekick::match::PhysicsEngine mPhysicsEngine;
                freekick::match::PhysicsState mPhysicsState;
                freekick::match::server::Dispatcher mDispatcher;
            public:

            private:

            public:


                // Private attribute accessor methods
                //  


                /**
                 * Set the value of mPause
                 * @param new_var the new value of mPause
                 */
                void setMPause ( bool new_var );

                /**
                 * Get the value of mPause
                 * @return the value of mPause
                 */
                bool getMPause ( );


                /**
                 * Set the value of mNewPhysicsEvents
                 * @param new_var the new value of mNewPhysicsEvents
                 */
                void setMNewPhysicsEvents ( freekick::match::PhysicsEventList new_var );

                /**
                 * Get the value of mNewPhysicsEvents
                 * @return the value of mNewPhysicsEvents
                 */
                freekick::match::PhysicsEventList getMNewPhysicsEvents ( );


                /**
                 * Set the value of mPhysicsEngine
                 * @param new_var the new value of mPhysicsEngine
                 */
                void setMPhysicsEngine ( freekick::match::PhysicsEngine new_var );

                /**
                 * Get the value of mPhysicsEngine
                 * @return the value of mPhysicsEngine
                 */
                freekick::match::PhysicsEngine getMPhysicsEngine ( );


                /**
                 * Set the value of mPhysicsState
                 * @param new_var the new value of mPhysicsState
                 */
                void setMPhysicsState ( freekick::match::PhysicsState new_var );

                /**
                 * Get the value of mPhysicsState
                 * @return the value of mPhysicsState
                 */
                freekick::match::PhysicsState getMPhysicsState ( );


                /**
                 * Set the value of mDispatcher
                 * @param new_var the new value of mDispatcher
                 */
                void setMDispatcher ( freekick::match::server::Dispatcher new_var );

                /**
                 * Get the value of mDispatcher
                 * @return the value of mDispatcher
                 */
                freekick::match::server::Dispatcher getMDispatcher ( );

            private:


                void initAttributes ( ) ;

            };
        }
    }
}

#endif // PHYSICS_H
