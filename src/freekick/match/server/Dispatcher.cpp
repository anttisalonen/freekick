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
  This file was generated on Sa Nov 22 2008 at 11:40:55
**************************************************************************/

#include "Dispatcher.h"

// Constructors/Destructors
//  

namespace freekick
{
    namespace match
    {
        namespace server
        {
            Dispatcher::Dispatcher (ClientListPtr clp, 
                                    addutil::network::Server* s, 
                                    boost::shared_ptr<Physics> p, 
                                    boost::shared_ptr<Rules> r,
                                    boost::shared_ptr<MatchStatus> ms)
                : mClientList(clp),
                  srv(*s),
                  mPhysics(p),
                  mRules(r),
                  mMatchStatus(ms)
            {
                mPhysics->subscribe(*this);
                mRules->subscribe(*this);
                // boost::thread t(boost::bind(&Dispatcher::run, this));
                last_physics_dispatch_time = boost::posix_time::microsec_clock::local_time();
            }

            Dispatcher::~Dispatcher ( ) 
            { 
                mPhysics->unsubscribe(*this);
                mRules->unsubscribe(*this);
            }

            void Dispatcher::run()
            {
                while(1)
                {
                    sleep(1);
                    // srv.broadcast("kpa\n");
                }
            }

            void Dispatcher::update(Physics* p)
            {
                PhysicsMessageList l;
                OwnerMessageList c;
                p->getUpdates(l, c);
                dispatchPhysicsMessages(l);
                // TODO: owner messages are not dispatched
            }

            void Dispatcher::update(Rules* r)
            {
                RulesMessageList l;
                ScoreMessageList s;
                r->getUpdates(l, s);
                dispatchRulesMessages(l);
                dispatchScoreMessages(s);
            }

            void Dispatcher::dispatchClientInformation ( ) 
            {

            }

            void Dispatcher::dispatchConnectionMessage (const ConnectionMessage& e ) 
            {

            }

            void Dispatcher::dispatchPhysicsMessage (const PhysicsMessage& e ) 
            {

            }

            void Dispatcher::dispatchPhysicsMessages (const PhysicsMessageList& es ) 
            {
                boost::posix_time::ptime this_time(boost::posix_time::microsec_clock::local_time());
                boost::posix_time::time_period diff_time(last_physics_dispatch_time, this_time);
                boost::posix_time::time_duration diff_dur = diff_time.length();
                unsigned long us_diff = diff_dur.total_microseconds();
                if(us_diff < 50000) return;
                last_physics_dispatch_time = this_time;

                // TODO: save and/or dispatch messages
                // default should be dispatch,
                // for efficiency only every x ms should a dispatch
                // occur, otherwise save updated entities internally
                // (in dispatcher) and dispatch everything later

                std::ostringstream oss(std::ostringstream::out);
                BOOST_FOREACH(PhysicsMessage m, es)
                {
                    oss << m.toString();
                }
                oss << "\n";
                srv.multicast(oss.str(), 1);  // TODO: enum instead of 1 (gid)
            }

            void Dispatcher::dispatchRulesMessages (const RulesMessageList& es ) 
            {
                if(es.size() < 1) return;
                std::ostringstream oss(std::ostringstream::out);
                BOOST_FOREACH(RulesMessage m, es)
                {
                    oss << m.toString();
                }
                oss << "\n";
                srv.multicast(oss.str(), 1);  // TODO: enum instead of 1 (gid)
            }

            void Dispatcher::dispatchConnectionMessages (const ConnectionMessageList& es)
            {
            }

            void Dispatcher::dispatchScoreMessages (const ScoreMessageList& es ) 
            {
                if(es.size() < 1) return;
                std::ostringstream oss(std::ostringstream::out);
                BOOST_FOREACH(ScoreMessage m, es)
                {
                    oss << m.toString();
                }
                oss << "\n";
                srv.multicast(oss.str(), 1);  // TODO: enum instead of 1 (gid)
            }

            void Dispatcher::newClientMessage(unsigned int clid, const messages::InitialDataRequest& m)
            {
                boost::shared_ptr<MatchData> md = mMatchStatus->getMatchData();
                if(!srv.is_connected(clid))
                {
                    std::cerr << "Dispatcher::newClientMessage: Trying to write to a non-existing Client ID\n";
                    return;
                }

                using namespace messages;
                boost::shared_ptr<InitialDataXMLMessage> icm(new InitialDataXMLMessage(md->getInitialDataXML()));
                try
                {
                    srv.write(icm->toString(), clid);
                }
                catch (...)
                {
                    std::cerr << "Dispatcher::newClientMessage: Client disconnected\n";
                }
            }

            void Dispatcher::newClientMessage(unsigned int clid, const messages::SetGeneralUpdateIntervalMessage& m)
            {
                ClientList::iterator it = mClientList->find(clid);
                if(it == mClientList->end() || !srv.is_connected(clid))
                {
                    std::cerr << "Dispatcher::newClientMessage: Trying to write to a non-existing Client ID\n";
                    return;
                }

                int req_interval = m.getValue();
                addutil::general::clamp(req_interval, min_dispatch_interval, max_dispatch_interval);
                int rest = req_interval % dispatch_interval_step;
                int corr_interval = req_interval + dispatch_interval_step - rest;
                it->second->setGeneralUpdateMessageInterval(corr_interval);

                const messages::GetGeneralUpdateInterval m2;
                newClientMessage(clid, m2);
            }

            void Dispatcher::newClientMessage(unsigned int clid, const messages::SetConstantUpdateIntervalMessage& m)
            {
                ClientList::iterator it = mClientList->find(clid);
                if(it == mClientList->end() || !srv.is_connected(clid))
                {
                    std::cerr << "Dispatcher::newClientMessage: Trying to write to a non-existing Client ID\n";
                    return;
                }

                int req_interval = m.getValue();
                addutil::general::clamp(req_interval, min_dispatch_interval, max_dispatch_interval);
                int rest = req_interval % dispatch_interval_step;
                int corr_interval = req_interval + dispatch_interval_step - rest;
                it->second->setGeneralUpdateMessageInterval(corr_interval);

                const messages::GetConstantUpdateInterval m2;
                newClientMessage(clid, m2);
            }

            void Dispatcher::newClientMessage(unsigned int clid, const messages::GetGeneralUpdateInterval& m)
            {
                ClientList::iterator it = mClientList->find(clid);
                if(it == mClientList->end() || !srv.is_connected(clid))
                {
                    std::cerr << "Dispatcher::newClientMessage: Trying to write to a non-existing Client ID\n";
                    return;
                }

                int interval = it->second->getGeneralUpdateMessageInterval();
                messages::GiveGeneralUpdateIntervalMessage m2(interval);
                try
                {
                    srv.write(m2.toString(), clid);
                }
                catch (...)
                {
                    std::cerr << "Dispatcher::newClientMessage::GetGeneralUpdateInterval: Client disconnected\n";
                }
            }

            void Dispatcher::newClientMessage(unsigned int clid, const messages::GetConstantUpdateInterval& m)
            {
                ClientList::iterator it = mClientList->find(clid);
                if(it == mClientList->end() || !srv.is_connected(clid))
                {
                    std::cerr << "Dispatcher::newClientMessage: Trying to write to a non-existing Client ID\n";
                    return;
                }

                int interval = it->second->getConstantUpdateMessageInterval();
                messages::GiveConstantUpdateIntervalMessage m2(interval);
                try
                {
                    srv.write(m2.toString(), clid);
                }
                catch (...)
                {
                    std::cerr << "Dispatcher::newClientMessage::GetConstantUpdateInterval: Client disconnected\n";
                }
            }

            void Dispatcher::sendPlayerList(unsigned int clid)
            {
                ClientList::iterator it = mClientList->find(clid);
                if(it == mClientList->end() || !srv.is_connected(clid))
                {
                    std::cerr << "Dispatcher::sendPlayerList: Trying to write to a non-existing Client ID\n";
                    return;
                }
                messages::ListOfPlayersMessage m(*(it->second->getControlledPlayers()));
                try
                {
                    srv.write(m.toString(), clid);
                }
                catch (...)
                {
                    std::cerr << "Dispatcher::sendPlayerList: Client disconnected\n";
                }                
            }
        }
    }
}

