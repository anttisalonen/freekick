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


#ifndef CONSOLE_H
#define CONSOLE_H



namespace freekick
{
    namespace match
    {
        namespace server
        {
            class Console
            {
            public:

                // Constructors/Destructors
                //  


                /**
                 * Empty Constructor
                 */
                Console ( );

                /**
                 * Empty Destructor
                 */
                virtual ~Console ( );



                /**
                 * @param  physics
                 * @param  rules
                 * @param  connectionlistener
                 */
                Console (freekick::match::server::Physics physics, freekick::match::server::Rules rules, freekick::match::server::ConnectionListener connectionlistener );


                /**
                 */
                void run ( );

            protected:

            public:

            protected:

            public:

            protected:



                /**
                 * @param  cc
                 */
                void getInput (freekick::match::server::ConsoleCommand cc ) const;

            private:

                // Private attributes
                //  

                bool mContinue;
                boost::shared_ptr<Physics> mPhysics;
                boost::shared_ptr<Rules> mRules;
                boost::shared_ptr<ConnectionListener> mConnectionListener;
            public:

            private:

            public:


                // Private attribute accessor methods
                //  


                /**
                 * Set the value of mContinue
                 * @param new_var the new value of mContinue
                 */
                void setMContinue ( bool new_var );

                /**
                 * Get the value of mContinue
                 * @return the value of mContinue
                 */
                bool getMContinue ( );


                /**
                 * Set the value of mPhysics
                 * @param new_var the new value of mPhysics
                 */
                void setMPhysics ( boost::shared_ptr<Physics> new_var );

                /**
                 * Get the value of mPhysics
                 * @return the value of mPhysics
                 */
                boost::shared_ptr<Physics> getMPhysics ( );


                /**
                 * Set the value of mRules
                 * @param new_var the new value of mRules
                 */
                void setMRules ( boost::shared_ptr<Rules> new_var );

                /**
                 * Get the value of mRules
                 * @return the value of mRules
                 */
                boost::shared_ptr<Rules> getMRules ( );


                /**
                 * Set the value of mConnectionListener
                 * @param new_var the new value of mConnectionListener
                 */
                void setMConnectionListener ( boost::shared_ptr<ConnectionListener> new_var );

                /**
                 * Get the value of mConnectionListener
                 * @return the value of mConnectionListener
                 */
                boost::shared_ptr<ConnectionListener> getMConnectionListener ( );

            private:


                void initAttributes ( ) ;

            };
        }
    }
}

#endif // CONSOLE_H
