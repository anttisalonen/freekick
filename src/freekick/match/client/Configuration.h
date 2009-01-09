/************************************************************************
  This file is part of Freekick.

  Freekick is free software: you can redistribute it and/or modify
  it under the terms of the GNU Affero General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  Freekick is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU Affero General Public License for more details.

  You should have received a copy of the GNU Affero General Public License
  along with Freekick.  If not, see <http://www.gnu.org/licenses/>.

  Copyright Antti Salonen, 2008
  This file was generated on So Okt 26 2008 at 12:09:20
**************************************************************************/


#ifndef CONFIGURATION_H
#define CONFIGURATION_H

#include <string>
#include <vector>

#include <boost/shared_ptr.hpp>

#include "addutil/network/IP_Connection.h"
#include "InputConfiguration.h"

/**
 * class Configuration
 */

namespace freekick
{
    namespace match
    {
        namespace client
        {
            class Configuration
            {
            public:

                /**
                 * @param  argc
                 * @param  argv
                 */
                Configuration (int argc, char** argv );
                addutil::network::IP_Connection getServerConnection ( ) const;
                boost::shared_ptr<InputConfiguration>& getInputConfiguration ( );

            private:

                addutil::network::IP_Connection server_connection;
                std::vector <int> player_ids;
                boost::shared_ptr<InputConfiguration> inputconfiguration; // TODO: make independent of cl_ogre

            };
        }
    }
}

#endif // CONFIGURATION_H
