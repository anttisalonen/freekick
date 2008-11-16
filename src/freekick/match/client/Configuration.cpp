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
  This file was generated on So Okt 26 2008 at 12:09:20
**************************************************************************/

#include "Configuration.h"

/**
 * @param  argc
 * @param  argv
 */
namespace freekick
{
    namespace match
    {
        namespace client
        {
            Configuration::Configuration (int argc, char** argv ) 
            {
                server_connection.ip_address = "localhost";
                server_connection.port = "32105";
            }

/**
 * @return const IP_Connection*
 */
            addutil::network::IP_Connection Configuration::getServerConnection ( ) const 
            {
                return server_connection;
            }

/**
 * @return const InputConfiguration*
 */
            cl_ogre::InputConfiguration* Configuration::getInputConfiguration ( ) const 
            {
                return inputconfiguration;
            }
        }
    }
}

