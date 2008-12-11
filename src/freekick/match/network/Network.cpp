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

#include "Network.h"

// Constructors/Destructors
//  

/**
 * @param  conf
 * @param  stat
 */

namespace freekick
{
    namespace match
    {
        namespace network
        {
            using namespace addutil::network;
            using namespace freekick;

            Network::Network (IP_Connection conn, MatchStatus* stat )
                : Client(conn),
                  status(stat)
            {
            }

            Network::~Network()
            {
            }

            void cut_from_string(const std::string& in, const std::string& start_token, const std::string& end_token, std::string& out)
            {
                using namespace std;
                out = "";
                size_t st = in.find(start_token);
                if(st == string::npos) throw "cut_from_string: start not found";
                size_t en = in.find(end_token, st + start_token.length());
                if(st == string::npos) throw "cut_from_string: end not found";
                out = in.substr(st + start_token.length(), en - st - start_token.length());
            }

            void Network::read(std::string buf)
            {
                std::string::size_type lf = buf.find('\n');
                buffer += buf;
                if(lf == std::string::npos)
                {
                    return;
                }

                if(handshake)
                {
                    std::cout << "Received: " << buf << std::endl;
                    write("FREEKICK_CLIENT \"Client with ncurses 0.15b\" \"0.2\" \"username/password\" H [2045,49]");
                    
                    Color c(1.0f, 0.0f, 0.0f);
                    status->addClub("club1");
                    status->addPlayer("club1", 2, c);
                    status->addPlayer("club1", 3, c);
                    std::cout << "Club 1 player ID: " << 2 << std::endl;
                    handshake = false;
                }
                else
                {
                    std::vector<std::string> read_strings;
                    splitstr_and_fill(read_strings, buffer, "\n");
                    buffer = read_strings.back();
                    read_strings.pop_back();
                    BOOST_FOREACH(std::string b, read_strings)
                    {
                        std::deque<std::string> events;
                        parse_events(b, events);
                        std::cout << "Number of events: " << events.size() << std::endl;

                        while(events.size() > 0)
                        {
                            // std::cout << events.front() << std::endl;
                            status->newEvent(events.front());
                            events.pop_front();
                        }
                    }
                }
            }

            bool Network::run ( ) 
            {
                std::cout << "Network engine running" << std::endl;
                handshake = true;
                connect();
                return true;
            }
        }
    }
}
