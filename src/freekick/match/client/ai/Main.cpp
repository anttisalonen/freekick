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
#include <boost/program_options.hpp>
#include <boost/exception.hpp>
#include <boost/date_time/posix_time/posix_time.hpp>

#include "addutil/Exception.h"

#include "client/Configuration.h"
#include "client/Network.h"
#include "client/ai/AI_Engine.h"
#include "client/ai/AIConfig.h"

#include "messages/InitialDataRequest.h"

using namespace freekick::match;
using namespace freekick::match::client;
using namespace freekick::match::client::ai_client;

int main(int argc, char** argv)
{
    try
    {
        boost::program_options::options_description desc("Parameters");
        desc.add_options()
            ("help", "show this help message")
            ("verbose,v", boost::program_options::value<int>(), "set verbosity")
            ;

        boost::program_options::variables_map vm;
        boost::program_options::store(boost::program_options::parse_command_line(argc, argv, desc), vm);
        boost::program_options::notify(vm);

        if (vm.count("verbose"))
        {
            std::cout << "Verbose level: " << vm["verbose"].as<int>() << ".\n";
            AIConfig::getInstance()->verbose = vm["verbose"].as<int>();
        }
        else
        {
            AIConfig::getInstance()->verbose = 0;
        }

        Configuration* configuration = new Configuration (argc, argv);
        addutil::network::IP_Connection conn = configuration->getServerConnection();
        Network* network;
        boost::shared_ptr<MatchStatus> status;
        boost::shared_ptr<AI_Engine> ai;
        std::cerr << "Freekick AI starting" << std::endl;
        try
        {
            network = new Network(conn, true);
            boost::thread network_thread(boost::bind(&Network::run, network, false));
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

        ai.reset(new ai_client::AI_Engine(status, network));
        network->setPlayerListHandler(ai);
        boost::thread ai_thread(boost::bind(&ai_client::AI_Engine::run, ai));

        ai_thread.join();
        network->disconnect();

        delete network;
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
    catch (const char* e)
    {
        std::cerr << "An exception has occurred:" << e << std::endl;
    }
    catch (...)
    {
        std::cerr << "Unknown exception has occurred." << std::endl;
    }

    return 0;
}
