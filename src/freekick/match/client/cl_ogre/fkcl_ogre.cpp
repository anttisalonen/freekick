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
#include <fstream>

#include <boost/thread/thread.hpp>
#include <boost/bind.hpp>
#include <boost/archive/text_oarchive.hpp>
#include <boost/exception.hpp>
#include <boost/date_time/posix_time/posix_time.hpp>

#include <Ogre.h>

#include "addutil/Exception.h"

#include "Input.h"
#include "InputHandler.h"
#include "Configuration.h"
#include "Network.h"
#include "Graphics.h"

#include "messages/InitialDataRequest.h"

using namespace freekick::match;
using namespace freekick::match::client;
using namespace freekick::match::client::cl_ogre;

void run_graphics(Graphics* g)
{
    g->run();
}

// TODO: make this less ugly
void run_network(Network* n)
{
    n->run();
}

int main(int argc, char** argv)
{
    int player_id = 0;
    if(argc == 1)
    {
        std::cerr << "No command line argument specifying player ID - being a spectator." << std::endl;
    }
    else
    {
        player_id = atoi(argv[1]);
        if(player_id < 1)
        {
            std::cerr << "Error: could not parse player ID from the command line.\n";
            exit(1);
        }
    }
    try
    {
        Configuration* configuration = new Configuration (argc, argv);
        addutil::network::IP_Connection conn = configuration->getServerConnection();
        Network* network;
        boost::shared_ptr<MatchStatus> status;
        std::cerr << "Freekick client starting" << std::endl;
        try
        {
            network = new Network(conn, false);
            boost::thread network_thread(boost::bind(&run_network, network));
            boost::this_thread::sleep(boost::posix_time::milliseconds(1000));  // TODO: make timeouts configurable?
            if(!network->is_connected())
            {
                std::cerr << "Network::Network: timeout while connecting";
                return 1;
            }
            network->sendMessage(messages::InitialDataRequest());
            boost::this_thread::sleep(boost::posix_time::milliseconds(1000));
            status = network->getMatchStatus();
            if(status.get() == 0)
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
        if(status.get() == 0) { std::cerr << "Received invalid match status?\n"; return 1; }
        Input* input = new Input(player_id, configuration, status.get(), network);
        Graphics* graphics = new Graphics(configuration, status.get(), input);

        // boost::thread status_thread(boost::bind(&run_status, status));
        boost::thread graphics_thread(boost::bind(&run_graphics, graphics));
        graphics_thread.join();
        std::cout << "Shutting down client.\n";

        delete graphics;
        delete input;
        delete network;
        delete configuration;
    }
    catch (boost::exception& e)
    {
	addutil::output_boost_exception(e);
    }
    catch(Ogre::Exception& e)
    {
#if OGRE_PLATFORM == PLATFORM_WIN32 || OGRE_PLATFORM == OGRE_PLATFORM_WIN32
        MessageBoxA(NULL, e.getFullDescription().c_str(), "An Ogre::exception has occurred!", MB_OK | MB_ICONERROR | MB_TASKMODAL);
#else
        std::cerr << "An Ogre::exception has occurred: " << e.getFullDescription().c_str() << std::endl;
#endif
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
