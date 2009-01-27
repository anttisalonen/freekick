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

#include "Server.h"

namespace addutil
{
    namespace network
    {
        Server::Server ()
            : ioserv(new boost::asio::io_service()),
              next_id(2)
        {
        }

        Server::~Server()
        {
            try
            {
                stopListening();
            }
            catch(...)
            {
            }
        }

        void Server::startListening(int port)
        {
            using boost::asio::ip::tcp;

            tcp::acceptor a(*ioserv, tcp::endpoint(tcp::v4(), port));
            try
            {
                while(1)
                {
                    boost::shared_ptr<tcp::socket> sock(new tcp::socket(*ioserv));
                    a.accept(*sock);
                    ConnectionPtr c(new Connection(next_id, sock));
                    connections[next_id] = c;
                    boost::thread t1(boost::bind(&Server::client_connected, this, next_id));
                    boost::thread t2(boost::bind(&Server::read_loop, this, c));
                    next_id++;
                }
            }
            catch (boost::exception& e)
            {
                output_boost_exception(e, "Server::startListening: A boost::exception has occurred");
            }
            catch (std::exception& e)
            {
                std::cerr << "Server::startListening: A std::exception has occurred: " << e.what() << std::endl;
            }
            catch (std::string& e)
            {
                std::cerr << "Server::startListening: An exception has occurred: " << e << std::endl;
            }
            catch (...)
            {
                std::cerr << "Server::startListening: Unknown exception has occurred." << std::endl;
            }
        }

        void Server::stopListening()
        {
            std::map<client_id, ConnectionPtr>::iterator it;
            for (it = connections.begin(); it != connections.end(); it++)
            {
                cleanup_client((*it).first);
            }
            connections.clear();
        }

        void Server::write(const std::string& msg, client_id id)
        {
            std::map<client_id, ConnectionPtr>::iterator it;
            it = connections.find(id);
            if(it == connections.end()) 
                throw "Server::write: Client ID not found\n";
            try
            {
                (*it).second->write(msg);
            }
            catch (boost::exception& e)
            {
                output_boost_exception(e, "Server::write: A boost::exception has occurred");
                cleanup_client(id);
            }
            catch(const char* e)
            {
                std::cerr << "Server::write: error: " << e << "; disconnecting.\n";
                cleanup_client(id);
            }
            catch(...)
            {
                std::cerr << "Server::write: unknown error; disconnecting." << std::endl;
                cleanup_client(id);
            }
        }

        void Server::disconnect(client_id id)
        {
            std::cerr << "Server::disconnect: disconnecting client " << id << std::endl;
            cleanup_client(id, true);
        }

        bool Server::is_connected(client_id id)
        {
            std::map<client_id, ConnectionPtr>::iterator it;
            it = connections.find(id);
            if(it == connections.end()) return false;
            return true;
        }

        void Server::cleanup_client(client_id id, bool already_notified)
        {
            // std::cerr << "Server::cleanup_client: cleaning up.\n";
            std::map<client_id, ConnectionPtr>::iterator it;
            it = connections.find(id);
            if(it != connections.end())   // already gone?
            {
                try
                {
                    (*it).second->disconnect();
                }
                catch(...)           // already gone
                {
                }
                connections.erase(it);
            }
            if(!already_notified)
                client_disconnected(id);
        }

        void Server::multicast(const std::string& msg, const std::set<client_id>& ids)
        {
            BOOST_FOREACH(client_id i, ids)
            {
                try
                {
                    write(msg, i);
                }
                catch(...)
                {
                    std::cerr << "Server::multicast: error while writing to client " << i << ".\n";
                }
            }
        }

        void Server::multicast(const std::string& msg, group_id gid)
        {
            GroupMap::iterator it;
            it = groups.find(gid);
            if (it == groups.end()) return;
            clientset* cs = &groups[gid];
            BOOST_FOREACH(client_id i, *cs)
            {
                try
                {
                    write(msg, i);
                }
                catch(...)
                {
                    std::cerr << "Server::multicast: error while writing to client " << i << ".\n";
                }
            }
        }

        void Server::add_to_group(client_id cid, group_id gid)
        {
            GroupMap::iterator it;
            it = groups.find(gid);
            if (it == groups.end()) groups[gid] = clientset();
            clientset::iterator it2;
            groups[gid].insert(cid);
        }

        void Server::remove_from_group(client_id cid, group_id gid)
        {
            GroupMap::iterator it;
            it = groups.find(gid);
            if (it == groups.end()) return;
            clientset::iterator it2 = it->second.find(cid);
            if (it2 != it->second.end())
                it->second.erase(it2);
        }

        void Server::broadcast(const std::string& msg)
        {
            typedef std::pair<client_id, ConnectionPtr> pair_cn;
            BOOST_FOREACH(pair_cn p, connections)
            {
                try
                {
                    p.second->write(msg);
                }
                catch(...)
                {
                    std::cerr << "Server::broadcast: error while writing to client " << p.first << ".\n";
                }
            }
        }

        void Server::read_loop(ConnectionPtr c)
        {
            while(c->connected())
            {
                boost::shared_ptr<std::string> n;
                try
                {
                    c->read(n);
                    client_input(c->getID(), *n);
                }
                catch (...)
                {
                    // std::cerr << "Server::read_loop: An exception has occurred." << std::endl;
                    break;
                }
            }
            cleanup_client(c->getID());
        }
    }
}
