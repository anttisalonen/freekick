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

                std::vector<std::string> read_strings;
                splitstr_and_fill(read_strings, buffer, "\n");
                buffer = read_strings.back();
                read_strings.pop_back();
                BOOST_FOREACH(std::string b, read_strings)
                {
                    if(handshake)
                    {
                        using namespace std;
                        std::cerr << "Handling handshake\n";
                        handshake = false;
                        std::deque<std::string> split_strings;
                        std::string curr_string;

                        splitstr_and_fill(split_strings, b, "homecl = Club {name = \"");
                        curr_string = split_strings.back();
                        split_strings.pop_back();
                        splitstr_and_fill(split_strings, curr_string, "awaycl = Club {name = \"");
                        curr_string = split_strings.back();
                        split_strings.pop_back();

                        splitstr_and_fill(split_strings, curr_string, "idnum = ");
                        std::cerr << "Split strings\n";

                        string c1name("c1"), c2name("c2");
                        vector<int> c1pls, c2pls;

                        split_strings.pop_front();
                        cut_from_string(split_strings[0], "\"", "\"", c1name);
                        split_strings.pop_front();
                        cut_from_string(split_strings[0], "\"", "\"", c2name);
                        split_strings.pop_front();
                        std::cout << "Club 1 name: " << c1name << std::endl;
                        std::cout << "Club 2 name: " << c2name << std::endl;

                        status->addClub(c1name);
                        status->addClub(c2name);
                        std::cerr << "Added clubs\n";

                        get_int(c1pls, split_strings, string("awaypl = [Player {"));
                        BOOST_FOREACH(int p, c1pls)
                        {
                            Color c(1.0f, 0.0f, 0.0f);
                            status->addPlayer(c1name, p, c);
                            cout << "Club 1 player ID: " << p << endl;
                        }

                        get_int(c2pls, split_strings, string("homepl = [Player {"));

                        BOOST_FOREACH(int p, c2pls)
                        {
                            Color c(0.0f, 0.0f, 1.0f);
                            status->addPlayer(c2name, p, c);
                            cout << "Club 2 player ID: " << p << endl;
                        }
                        std::cerr << "Added players\n";
                        write("\nOK []\n");
                        std::cerr << "Wrote OK\n";
                    }
                    else
                    {
                        std::deque<std::string> events;
                        parse_events(b, events);

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
