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
**************************************************************************/

#include <cstdlib>

#include <iostream>
#include <exception>

#include <boost/thread/thread.hpp>
#include <boost/bind.hpp>

#include "freekick/match/server/ServerManager.h"
#include "MatchStatus.h"

using namespace addutil;
using namespace freekick;

using freekick::match::MatchStatus;
using freekick::match::server::ServerManager;

void run_status(MatchStatus* s)
{
    s->run();
}

int main(int argc, char** argv)
{
    try
    {
        MatchStatus* status = new MatchStatus();
        std::cerr << "Starting Freekick server" << std::endl;
        ServerManager sm(32105);

        delete status;
    }
    catch (boost::exception& e)
    {
        std::cerr << "A boost::exception has occurred: " << e.diagnostic_information() << std::endl;
    }
    catch (std::exception& e)
    {
        std::cerr << "A std::exception has occurred: " << e.what() << std::endl;
    }
    catch (std::string& e)
    {
        std::cerr << "An exception has occurred: " << e << std::endl;
    }
    catch (...)
    {
        std::cerr << "Unknown exception has occurred." << std::endl;
    }

    return 0;
}
