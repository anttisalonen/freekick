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

#include "Physics.h"

// Constructors/Destructors
//  

namespace freekick
{
    namespace match
    {
        namespace server
        {
            Physics::Physics (boost::shared_ptr<MatchStatus> ms)
                : mMatchStatus(ms)
                , mPhysicsEngine(new BulletPhysicsEngine(Vector3(-50,-20,-50), 
                                                         Vector3(200, 100, 200), 
                                                         500, Vector3(0, -9.8, 0)))
            {
                mPhysicsEngine->subscribe(*this);
                mPhysicsEngine->addStaticBoxObject(PitchID, Vector3(200, 50, 140), Vector3(100, -50, 70));
                mPhysicsEngine->addDynamicSphereObject(BallID, 1.0f, 5.0f, Vector3(10, 50, 10));
                std::vector<int> hplayers;
                std::vector<int> aplayers;
                boost::shared_ptr<Club> club1 = mMatchStatus->getMatchData()->getHomeClub();
                boost::shared_ptr<Club> club2 = mMatchStatus->getMatchData()->getAwayClub();
                club1->getPlayerIDs(hplayers);
                club2->getPlayerIDs(aplayers);
                BOOST_FOREACH(int idnum, hplayers)
                {
                    Vector3 loc(idnum % 90 + 1, idnum % 20 + 1, idnum % 50 + 1);
                    mPhysicsEngine->addControllableObject(idnum, Vector3(1.0f, 2.0f, 1.0f), 80.0f, loc);
                }
                BOOST_FOREACH(int idnum, aplayers)
                {
                    Vector3 loc(idnum % 90 + 1, idnum % 20 + 1, idnum % 50 + 1);
                    mPhysicsEngine->addControllableObject(idnum, Vector3(1.0f, 2.0f, 1.0f), 80.0f, loc);
                }
            }

            Physics::~Physics ( ) 
            {
                mPhysicsEngine->unsubscribe(*this);
            }

            void Physics::setPause ( bool new_var ) 
            {
                mPause = new_var;
            }

            bool Physics::getPause ( ) 
            {
                return mPause;
            }

            bool Physics::run ( ) 
            {
                using namespace boost::posix_time;
                unsigned long sleep_time = 1000000/60;
                while(1)
                {
                    ptime before_time(microsec_clock::local_time());
                    mPhysicsEngine->stepWorld(1.0f/60.0f);
                    ptime after_time(microsec_clock::local_time());
                    time_period diff_time(before_time, after_time);
                    time_duration diff_dur = diff_time.length();
                    long us_diff = diff_dur.total_microseconds();
                    long time_left = sleep_time - us_diff;
                    if(time_left > 0)
                        boost::this_thread::sleep(microseconds(time_left));
                }
                return true;
            }

            void Physics::newClientMessage (const messages::MovePlayerControlMessage& e ) 
            {
                using namespace messages;
                addutil::Vector3 v;
                e.getTargetVector(v);
                if (!mPhysicsEngine->setObjectVelocity(e.getPlayerID(), v))
                {
                    std::cerr << "Physics::newClientMessage: Invalid player ID\n";
                }
            }

            void Physics::update(PhysicsEngine* e)
            {
                EntityPtrMap m;
                e->getUpdatedObjects(m);
                typedef std::pair<ObjectID, EntityPtr> pair_en;
                BOOST_FOREACH(pair_en p, m)
                {
                    const addutil::Vector3 pos = p.second->getPosition();
                    const addutil::Quaternion orien = p.second->getOrientation();
                    messages::ConstantUpdateMessage c(p.first, 0, pos, orien);
                    newmessages.push_back(c);
                }
                publish();
                mMatchStatus->update(newmessages);
                clearMessages();
            }

            void Physics::clearMessages()
            {
                newmessages.clear();
            }

            void Physics::getUpdates(PhysicsMessageList& l)
            {
                l = newmessages;
            }
        }
    }
}

