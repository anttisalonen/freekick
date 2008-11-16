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

#include <Ogre.h>

#include "Input.h"
#include "InputHandler.h"
#include "Configuration.h"
#include "Network.h"
#include "Graphics.h"

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

void run_status(MatchStatus* s)
{
    s->run();
}

int main(int argc, char** argv)
{
    bool start_graphics = true;
    if(argc > 1)
    {
        if(!std::strcmp(argv[1], "-n"))
        {
            start_graphics = false;
        }
    }
    try
    {
        Configuration* configuration = new Configuration (argc, argv);
        addutil::network::IP_Connection conn = configuration->getServerConnection();
        MatchStatus* status = new MatchStatus();
        Network* network = new Network(conn, status);
        Input* input = new Input(configuration, status, network);
        Graphics* graphics = new Graphics(configuration, status, input);

        std::cerr << "Freekick client starting" << std::endl;
        // boost::thread status_thread(boost::bind(&run_status, status));
        if(start_graphics)
        {
            boost::thread network_thread(boost::bind(&run_network, network));
            boost::thread graphics_thread(boost::bind(&run_graphics, graphics));
            graphics_thread.join();
        }
        else
        {
            boost::thread network_thread(boost::bind(&run_network, network));
            network_thread.join();
        }

        delete graphics;
        delete input;
        delete network;
        delete status;
        delete configuration;
    }
    catch (boost::exception& e)
    {
        std::cerr << "A boost::exception has occurred: " << e.diagnostic_information() << std::endl;
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
