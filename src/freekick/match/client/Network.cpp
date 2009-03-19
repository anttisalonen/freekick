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
        namespace client
        {
            using namespace addutil::network;
            using namespace freekick;

            Network::Network (IP_Connection conn, bool ai)
                : Client(conn),
                  constant_update_interval_ms(50),
                  general_update_interval_ms(1000),
                  aicontroller(ai)
            {
            }

            Network::~Network()
            {
            }

            boost::shared_ptr<freekick::match::MatchStatus> Network::getMatchStatus() const
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
                std::ostringstream oss(std::ostringstream::out);
                oss << m.toString() << "\n";
                write(oss.str());
            }

            void Network::sendMessages(const std::vector<boost::shared_ptr<messages::Message> >& ms)
            {
                if(ms.size() < 1) return;
                std::ostringstream oss(std::ostringstream::out);
                BOOST_FOREACH(boost::shared_ptr<messages::Message> m, ms)
                {
                    oss << m->toString() << "\n";
                }
                oss << "\n";
                write(oss.str());
            }

            void Network::disconnect()
            {
                Client::disconnect();
            }

            void Network::read(std::string buf)
            {
                using namespace messages;

                // std::cerr << "Network::read: New data received\n";

                // summing up one entire line
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
                    std::string init("FREEKICK_CLIENT \"OgreClient 0.0.2\" \"0.2\" \"username/password\" ");
                    init += (aicontroller ? "A" : "H");
                    write(init);
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
                            if(st == initialdata_xml_id)
                            {
                                try
                                {
                                    std::cout << "Received initial data XML message\n";
                                    const messages::InitialDataXMLMessage idm(this_event);
                                    std::cout << "Parsed initial data XML message\n";
                                    status = idm.getMatchStatus();
                                }
                                catch(std::exception& e)
                                {
                                    std::cerr << "Error when parsing initial data XML message:" << e.what() << std::endl;
                                }
                                continue;
                            }
                        }

                        else if(status.get() == 0)
                            continue;

                        if (t == s_const_upd)
                        {
                            try
                            {
                                const messages::ConstantUpdateMessage m(this_event);
                                status->update(m, constant_update_interval_ms * 0.001f);
                            }
                            catch(...)
                            {
                                std::cerr << "Network: failed to parse ConstantUpdateMessage.\n";
                            }
                            continue;
                        }
                        else if (t == s_gen_status_upd)
                        {
                            try
                            {
                                const messages::GeneralUpdateStatusMessage m(this_event);
                                status->update(m);
                            }
                            catch(...)
                            {
                                std::cerr << "Network: failed to parse GeneralUpdateStatusMessage.\n";
                            }
                            continue;
                        }
                        else if (t == s_gen_score_upd)
                        {
                            try
                            {
                                const messages::GeneralUpdateScoreMessage m(this_event);
                                status->update(m);
                            }
                            catch(...)
                            {
                                std::cerr << "Network: failed to parse GeneralUpdateScoreMessage.\n";
                            }
                            continue;
                        }
                        else if (t == s_give_gen_upd_int)
                        {
                            try
                            {
                                const messages::GiveGeneralUpdateIntervalMessage m(this_event);
                                general_update_interval_ms = m.getValue();
                            }
                            catch(...)
                            {
                                std::cerr << "Network: failed to parse GiveGeneralUpdateIntervalMessage.\n";
                            }
                            continue;
                        }
                        else if (t == s_give_const_upd_int)
                        {
                            try
                            {
                                const messages::GiveConstantUpdateIntervalMessage m(this_event);
                                constant_update_interval_ms = m.getValue();
                            }
                            catch(...)
                            {
                                std::cerr << "Network: failed to parse GiveConstantUpdateIntervalMessage.\n";
                            }
                            continue;
                        }
                        else if (t == s_list_of_players)
                        {
                            try
                            {
                                const messages::ListOfPlayersMessage m(this_event);
                                if(plh)
                                    plh->newListOfPlayers(m.getList());
                            }
                            catch(...)
                            {
                                std::cerr << "Network: failed to parse ListOfPlayersMessage.\n";
                            }
                        }
                        else
                        {
                            std::cerr << "Network::read: received an unknown message.\n";
                        }
                    }
                }
            }

            bool Network::run (bool async) 
            {
                try
                {
                    std::cout << "Network engine running" << std::endl;
                    handshake = true;
                    connect(async);
                    return true;
                }
                catch (...)
                {
                    throw std::runtime_error("Network::run: unknown exception.");
                }
            }

            int Network::getConstantUpdateInterval() const
            {
                return constant_update_interval_ms;
            }

            int Network::getGeneralUpdateInterval() const
            {
                return general_update_interval_ms;
            }

            void Network::setPlayerListHandler(const boost::shared_ptr<PlayerListHandler>& p)
            {
                plh = p;
            }
        }
    }
}
