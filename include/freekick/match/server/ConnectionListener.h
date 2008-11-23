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


#ifndef CONNECTIONLISTENER_H
#define CONNECTIONLISTENER_H





namespace freekick
{
    namespace match
    {
        namespace server
        {
            class ConnectionListener
            {
            public:

                // Constructors/Destructors
                //  


                /**
                 * Empty Constructor
                 */
                ConnectionListener ( );

                /**
                 * Empty Destructor
                 */
                virtual ~ConnectionListener ( );



                /**
                 * @param  c
                 */
                void newClient (freekick::match::Client c );

            protected:

            public:

            protected:

            public:

            protected:


            private:

                // Private attributes
                //  

                io_service m_mIoService;
                freekick::match::ClientList m_mClientList;
                freekick::match::server::Dispatcher m_mDispatcher;
            public:

            private:

            public:


                // Private attribute accessor methods
                //  


                /**
                 * Set the value of m_mIoService
                 * @param new_var the new value of m_mIoService
                 */
                void setMIoService ( io_service new_var );

                /**
                 * Get the value of m_mIoService
                 * @return the value of m_mIoService
                 */
                io_service getMIoService ( );


                /**
                 * Set the value of m_mClientList
                 * @param new_var the new value of m_mClientList
                 */
                void setMClientList ( freekick::match::ClientList new_var );

                /**
                 * Get the value of m_mClientList
                 * @return the value of m_mClientList
                 */
                freekick::match::ClientList getMClientList ( );


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

#endif // CONNECTIONLISTENER_H
