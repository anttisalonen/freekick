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
#include <fstream>

#include <boost/thread/thread.hpp>
#include <boost/bind.hpp>
#include <boost/exception.hpp>
#include <boost/date_time/posix_time/posix_time.hpp>

#include "addutil/Exception.h"

#include "Configuration.h"
#include "Network.h"

#include "messages/InitialDataRequest.h"

using namespace freekick::match;
using namespace freekick::match::client;

// TODO: make this less ugly
void run_network(Network* n)
{
    n->run();
}

int main(int argc, char** argv)
{
    try
    {
        Configuration* configuration = new Configuration (argc, argv);
        addutil::network::IP_Connection conn = configuration->getServerConnection();
        Network* network;
        MatchStatus* status;
        std::cerr << "Freekick AI starting" << std::endl;
        try
        {
            network = new Network(conn);
            boost::thread network_thread(boost::bind(&run_network, network));
            boost::this_thread::sleep(boost::posix_time::milliseconds(2000));  // TODO: make timeouts configurable?
            if(!network->is_connected())
            {
                std::cerr << "Network::Network: timeout while connecting";
                return 1;
            }
            network->sendMessage(messages::InitialDataRequest());
            boost::this_thread::sleep(boost::posix_time::milliseconds(2000));
            status = network->getMatchStatus();
            if(status == 0)
            {
                std::string err("Network::Network: no match status created.\n");
                std::cerr << err;
                return 1;
            }
            else std::cerr << "Network::Network: Success.\n";
        }
        catch(...)
        {
            std::cerr << "Network connection failed; exiting.\n";
            return 1;
        }
        if(status == 0) { std::cerr << "Received invalid match status?\n"; return 1; }
        ai_client::AI_Engine* ai = new ai_client::AI_Engine(status, network);
        boost::thread ai_thread(boost::bind(&ai_client::AI_Engine::run, &ai));

        // boost::thread status_thread(boost::bind(&run_status, status));
        ai_thread.join();
        network_thread.join();

        delete ai;
        delete network;
        delete status;
        delete configuration;
    }
    catch (boost::exception& e)
    {
	addutil::output_boost_exception(e);
    }
    catch (std::exception& e)
    {
        std::cerr << "A std::exception has occurred: " << e.what() << std::endl;
    }
    catch (...)
    {
        std::cerr << "Unknown exception has occurred." << std::endl;
    }

    return 0;
}
