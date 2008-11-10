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


#ifndef CONFIGURATION_H
#define CONFIGURATION_H

#include <string>
#include <vector>
#include "IP_Connection.h"
#include "InputConfiguration.h"

/**
  * class Configuration
  */

class Configuration
{
public:

    /**
     * @param  argc
     * @param  argv
     */
    Configuration (int argc, char** argv );

    /**
     * @return const IP_Connection*
     */
    IP_Connection getServerConnection ( ) const;

    /**
     * @return const InputConfiguration*
     */
    InputConfiguration* getInputConfiguration ( ) const;

private:

    IP_Connection server_connection;
    std::vector <int> player_ids;
    InputConfiguration* inputconfiguration;

};

#endif // CONFIGURATION_H
