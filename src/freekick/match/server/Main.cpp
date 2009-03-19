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
**************************************************************************/

#include <cstdlib>

#include <iostream>
#include <exception>

#include <boost/thread/thread.hpp>
#include <boost/bind.hpp>

#include "addutil/Exception.h"

#include "freekick/match/server/ServerManager.h"
#include "Club.h"
#include "Player.h"
#include "Lineup.h"
#include "MatchStatus.h"

using namespace addutil;
using namespace freekick;

using namespace freekick::soccer;
using freekick::match::MatchStatus;
using freekick::match::server::ServerManager;

int main(int argc, char** argv)
{
    try
    {
        if(argc == 1)
        {
            std::cerr << "No command line argument - please specify a matchdata file to load." << std::endl;
            exit(1);
        }
        boost::shared_ptr<MatchData> data(new MatchData(argv[1]));
        boost::shared_ptr<MatchStatus> status(new MatchStatus(data));

        std::cerr << "Starting Freekick server" << std::endl;
        ServerManager sm(32105, status, 
                         "Freekick C++ Server", 
                         "Welcome to the world's second Freekick server!\nHopefully you enjoy your stay.");
        sm.run();
    }
    catch (boost::exception& e)
    {
        addutil::output_boost_exception(e);
    }
    catch (std::exception& e)
    {
        std::cerr << "A std::exception has occurred: " << e.what() << std::endl;
    }
    catch (std::string& e)
    {
        std::cerr << "An exception has occurred: " << e << std::endl;
    }
    catch (const char* e)
    {
        std::cerr << "An exception has occurred: " << e << std::endl;
    }
    catch (...)
    {
        std::cerr << "Unknown exception has occurred." << std::endl;
    }

    return 0;
}
