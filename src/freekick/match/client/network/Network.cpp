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

            Network::Network (IP_Connection conn)
                : Client(conn),
                  status(0)
            {
            }

            Network::~Network()
            {
            }

            freekick::match::MatchStatus* Network::getMatchStatus()
            {
                return status;
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

            void Network::sendMessage(const messages::Message& m)
            {
                write(m.toString());
            }

            void Network::read(std::string buf)
            {
                using namespace messages;

                // std::cerr << "Network::read: New data received\n";

                // summing up one entire line - TODO: handle serialization
                std::string::size_type lf = buf.find('\n');
                buffer += buf;
                if(lf == std::string::npos)
                {
                    return;
                }

                // buf is the entire line
                // std::cout << "Received: " << buf << std::endl;

                if(handshake)
                {
                    write("FREEKICK_CLIENT \"OgreClient 0.0.??\" \"0.2\" \"username/password\" H [101]");
                    handshake = false;
                    return;
                }

                std::vector<std::string> read_strings;
                splitstr_and_fill(read_strings, buffer, "\n");

                // buffer is the internal buffer that holds unfinished lines
                buffer = read_strings.back();

                // read_strings holds complete lines
                read_strings.pop_back();
                BOOST_FOREACH(std::string b, read_strings)
                {
                    std::deque<std::string> events;
                    parse_events(b, events, true);
                    // std::cout << "Number of events: " << events.size() << std::endl;
                    while(events.size() > 0)
                    {
                        std::string t;
                        std::string this_event = events.front();
                        events.pop_front();
                        // std::cout << "Message: " << this_event << std::endl;
                        try
                        {
                            t = getMessageType(this_event);
                        }
                        catch(...)
                        {
                            std::cerr << "Network::read: received invalid message.\n";
                            continue;
                        }

                        if (t == serialization_delim)
                        {
                            int st = getSerializationMessageType(this_event);
                            if(st == initialdata_club_id)
                            {
                                std::cout << "Received initial data club message\n";
                                const messages::InitialDataClubMessage idm(this_event);
                                std::cout << "Parsed initial data club message\n";
                                boost::shared_ptr<Club> c1, c2;
                                idm.getClub(true, c1);
                                idm.getClub(false, c2);
                                boost::shared_ptr<Stadium> stad(new Stadium());

                                boost::shared_ptr<MatchData> md(new MatchData(c1, c2, stad));
                                std::cout << "Created matchdata\n";
                                status = new MatchStatus(md);
                                std::cout << "\n--------------------------\nParsed initial data club message.\n----------------------------------\n";
                                continue;
                            }
                            if(status != 0)
                            {
                                boost::shared_ptr<MatchData> md = status->getMatchData();

                                if(st == initialdata_kit_id)
                                {
                                    continue;
                                }
                            }
                            // TODO: parse all possible serialization messages
                        }

                        if(status != 0)
                        {
                            if (t == s_const_upd)
                            {
                                try
                                {
                                    const messages::ConstantUpdateMessage m(this_event);
                                    status->update(m);
                                }
                                catch(...)
                                {
                                    std::cerr << "Network: failed to parse ConstantUpdateMessage.\n";
                                }
                                continue;
                            }
                            else
                            {
                                std::cerr << "Network::read: received an unknown message.\n";
                            }
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
