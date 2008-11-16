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
                : status(stat), 
                  ip_conn(conn), 
                  resolver(io_service), 
                  serversocket(io_service)
            {
                connect();
            }

            void Network::connect()
            {
                using boost::asio::ip::tcp;
                if (serversocket.is_open()) serversocket.close();
                tcp::resolver::query query(tcp::v4(), ip_conn.ip_address, ip_conn.port);
                resolver.async_resolve(query, 
                                       boost::bind(&Network::handle_resolve, this, 
                                                   boost::asio::placeholders::error, 
                                                   boost::asio::placeholders::iterator));
            }

            Network::~Network()
            {
                serversocket.close();
            }

            void Network::handle_resolve(const boost::system::error_code& err, 
                                         boost::asio::ip::tcp::resolver::iterator endpoint_iterator)
            {
                if (!err)
                {
                    boost::asio::ip::tcp::endpoint endpoint = *endpoint_iterator;
                    serversocket.async_connect(endpoint, 
                                               boost::bind(&Network::handle_connect, this, 
                                                           boost::asio::placeholders::error,
                                                           ++endpoint_iterator));
                }
                else
                {
                    std::cerr << "Error at resolve: " << err.message() << std::endl;
                }
            }

            void Network::handle_connect(const boost::system::error_code& err, 
                                         boost::asio::ip::tcp::resolver::iterator endpoint_iterator)
            {
                if (!err)
                {
                    boost::asio::async_read_until(serversocket, serverdata, '\n',
                                                  boost::bind(&Network::handle_read_handshake, this,
                                                              boost::asio::placeholders::error));
                }
                else if (endpoint_iterator != boost::asio::ip::tcp::resolver::iterator())
                {
                    serversocket.close();
                    boost::asio::ip::tcp::endpoint endpoint = *endpoint_iterator;
                    serversocket.async_connect(endpoint, 
                                               boost::bind(&Network::handle_connect, this,
                                                           boost::asio::placeholders::error,
                                                           ++endpoint_iterator));
                }
                else
                {
                    std::cerr << "Error at connect: " << err.message() << std::endl;
                }
            }

            template <typename ContT> void splitstr_and_fill(ContT& out, std::string& in, const std::string& split)
            {
                using namespace std;
                using namespace boost;
                using namespace boost::algorithm;
                typedef split_iterator<string::iterator> string_split_iterator;

                for(split_iterator<string::iterator> It = make_split_iterator(in, first_finder(split, is_equal()));
                    It!=string_split_iterator();
                    ++It)
                {
                    out.push_back(copy_range<std::string>(*It));
                }
            }

            void cut_from_string(const std::string& in, const std::string& start_token, const std::string& end_token, std::string& out)
            {
                using namespace std;
                out = "";
                size_t st = in.find(start_token);
                if(st == string::npos) throw "func2: start not found";
                size_t en = in.find(end_token, st + start_token.length());
                if(st == string::npos) throw "func2: end not found";
                out = in.substr(st + start_token.length(), en - st - start_token.length());
            }

            template <typename Cont1T, typename Cont2T, typename StringT>
            void get_int(Cont1T& nums, Cont2T& strings, const StringT& fin)
            {
                using namespace std;
                bool finished = false;
                while(!finished && strings.size() > 0)
                {
                    finished = (strings[0].find(fin) != string::npos);
                    string& this_string = strings[0];
                    string::iterator it = this_string.begin();
                    string num("");
                    while(*it >= '0' && *it <= '9') num += *it++;
                    nums.push_back(boost::lexical_cast<int>(num));
                    strings.pop_front();
                }
            }

            void Network::handle_read_handshake(const boost::system::error_code& err)
            {
                if (!err)
                {
                    std::istream input_stream(&serverdata);
                    std::string buffer;

/*
  while(!input_stream.eof())
  {
  char chb[1024];
  input_stream.read(chb, 1024);
  buffer += chb;
  }
  std::cout << buffer << std::endl;
*/

                    while (std::getline(input_stream, buffer) && buffer != "\n")
                    {
                        using namespace std;
                        std::deque<std::string> split_strings;
                        std::string curr_string;
                        splitstr_and_fill(split_strings, buffer, "homecl = Club {name = \"");
                        curr_string = split_strings.back();
                        split_strings.pop_back();
                        splitstr_and_fill(split_strings, curr_string, "awaycl = Club {name = \"");
                        curr_string = split_strings.back();
                        split_strings.pop_back();
                        splitstr_and_fill(split_strings, curr_string, "idnum = ");

                        string c1name, c2name;
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

/*
  std::string::size_type cl1 = buffer.find_first_of("homecl = Club {name = \"");
  std::string::size_type cl2 = buffer.find_first_of("awaycl = Club {name = \"");

  if (cl1 != std::string::npos)
  {
  std::string::size_type cl1name_end = buffer.find_first_of("\"", cl1 + 24);
  std::string cl1name = buffer.substr(cl1, cl1name_end);
  std::cout << "Home club: " << cl1name << std::endl;
  status->addClub(cl1name);

  std::string::size_type pls1 = buffer.find_first_of("homepl = [Player {");
  std::string::size_type pls2 = buffer.find_first_of("awaypl = [Player {", pls1);

  std::string::size_type it = pls1;
  while (it < pls2)
  {
  std::string::size_type id = buffer.find_first_of("idnum = ", it);
  std::string::size_type idend = buffer.find_first_of(" ", id + 9);
  std::string num = buffer.substr(id, idend);
  int idnum = boost::lexical_cast<int> (num);
  status->addPlayer(cl1name, idnum);
  std::cout << "Added player: " << idnum << std::endl;
  }
  }
*/
                    }

                    std::ostream send_stream(&senddata);
                    send_stream << "\nOK []\n";

                    boost::asio::async_write(serversocket, senddata,
                                             boost::bind(&Network::handle_write_data, this,
                                                         boost::asio::placeholders::error));

                }
                else
                {
                    std::cerr << "Error at read_handshake: " << err.message() << std::endl;
                }
            }

            void Network::handle_write_data(const boost::system::error_code& err)
            {
                if (!err)
                {
                    // handle_read_events(err, 0);

                    boost::asio::async_read_until(serversocket, serverdata, "\n\n",
                                                  boost::bind(&Network::handle_read_events, this,
                                                              boost::asio::placeholders::error,
                                                              boost::asio::placeholders::bytes_transferred));

                }
                else
                {
                    std::cerr << "Error at write_data: " << err.message() << std::endl;
                }
            }

            void Network::handle_read_events(const boost::system::error_code& err, std::size_t bytes)
            {
                if (!err)
                {
                    // std::cout << &serverdata;

                    std::istream datastream(&serverdata);
                    std::deque<std::string> events;

                    boost::asio::streambuf::const_buffers_type bufs = serverdata.data();
                    std::string line(boost::asio::buffers_begin(bufs),
                                     boost::asio::buffers_begin(bufs) + bytes);
                    serverdata.consume(bytes);

                    parse_events(line, events);
                    line = "";

/*
  if(events.size() > 0)
  {
  int length = 0;
  std::deque<std::string>::iterator sit;
  for (sit = events.begin(); sit != events.end(); sit++)
  length += sit->length();
  boost::posix_time::ptime this_time = boost::posix_time::microsec_clock::universal_time();
  std::cout << this_time << " New Event List!: " << length << std::endl;
  }
*/

                    while(events.size() > 0)
                    {
                        // std::cout << events.front() << std::endl;
                        status->newEvent(events.front());
                        events.pop_front();
                    }

                    // std::cout << "Waiting for events...\n";
                    boost::asio::async_read_until(serversocket, serverdata, "\n\n",
                                                  boost::bind(&Network::handle_read_events, this,
                                                              boost::asio::placeholders::error,
                                                              boost::asio::placeholders::bytes_transferred));

/*
  boost::asio::async_read(serversocket, serverdata, 
  boost::asio::transfer_at_least(1024),
  boost::bind(&Network::handle_read_events, this,
  boost::asio::placeholders::error));
*/
                }
                else
                {
                    std::cerr << "Error at read_events: " << err.message() << std::endl;
                }
            }

/**
 * @return bool
 * @param  data
 */
            bool Network::sendData (const std::string* data ) 
            {
                return true;
            }

/**
 */
            bool Network::run ( ) 
            {
                std::cout << "Network engine running" << std::endl;
                io_service.run();
/*
  try
  {
  boost::asio::streambuf streambuffer;
  boost::asio::read_until(*serversocket, streambuffer, '\n');
  std::cout << &streambuffer;  // match info
  boost::asio::streambuf reply;
  std::ostream reply_stream(&reply);
  reply_stream << "\nOK\n";
  boost::asio::write(*serversocket, reply);
  while(1)
  {
  boost::asio::async_read_until(*serversocket, streambuffer, '\n');
  std::istream datastream(&streambuffer);
  std::vector<std::string> events;
  parse_events(datastream, events);
  while(events.size() > 0)
  {
  std::cout << events.back() << std::endl;
  events.pop_back();
  }

  std::string buf = "";
  char c;
  while(datastream.good())
  {
  c = datastream.get();
  buf += c;
  }
  std::cout << buf << std::endl;
  }
  }
  catch (std::exception& e)
  {
  std::cerr << "Exception at Network::run: " << e.what() << std::endl;
  serversocket->close();
  std::cerr << "Retrying in 10 seconds..." << std::endl;
  sleep(10);
  try
  {
  connect();
  }
  catch (std::exception& e)
  {
  std::cerr << "Exception while reconnecting: " << e.what() << std::endl;
  return false;
  }
  return run();
  } */
                return true;
            }

/**
 * @return string*
 */
            std::string* Network::readData ( ) 
            {
                return 0;
            }
        }
    }
}
