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
                p->getUpdates(l);
                dispatchPhysicsMessages(l);
            }

            void Dispatcher::update(Rules* r)
            {
                RulesMessageList l;
                r->getUpdates(l);
                dispatchRulesMessages(l);
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
                std::ostringstream oss(std::ostringstream::out);
                BOOST_FOREACH(RulesMessage m, es)
                {
                    oss << m.toString();
                }
                oss << "\n";
                srv.multicast(oss.str(), 1);  // TODO: enum instead of 1 (gid)
            }

            void Dispatcher::dispatchConnectionMessages (const ConnectionMessageList& es ) 
            {

            }

            void Dispatcher::newClientMessage(unsigned int clid, const messages::InitialDataRequest& m)
            {
                boost::shared_ptr<MatchData> md = mMatchStatus->getMatchData();
                if(!srv.is_connected(clid))
                {
                    std::cerr << "Dispatcher::newClientMessage: Trying to write to a non-existing Client ID\n";
                    return;
                }

                // TODO: get actual colors of the clubs, goalkeepers and referee
                boost::shared_ptr<Club> club1 = md->getHomeClub();
                boost::shared_ptr<Club> club2 = md->getAwayClub();
                using addutil::Color;
                Kit temporarykit(0, Color(1.0f, 1.0f, 0.0f), Color(0.5f, 0.5f, 1.0f), Color(0.0f, 0.1f, 0.2f), Color(1.0f, 0.0f, 1.0f));

                std::vector<boost::shared_ptr<messages::Message> > ms;
                boost::shared_ptr<messages::InitialDataClubMessage> icm1(new messages::InitialDataClubMessage(club1, club2));
                boost::shared_ptr<messages::InitialDataKitMessage>  icm2(new messages::InitialDataKitMessage (temporarykit, temporarykit, temporarykit, temporarykit, temporarykit));
                ms.push_back(icm1);
                ms.push_back(icm2);

                try
                {
                    BOOST_FOREACH(boost::shared_ptr<messages::Message> m, ms)
                        srv.write(m->toString(), clid);
                }
                catch (...)
                {
                    std::cerr << "Dispatcher::newClientMessage: Client disconnected\n";
                }
            }
        }
    }
}

