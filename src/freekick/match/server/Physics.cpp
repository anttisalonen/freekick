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
            Physics::Physics (boost::shared_ptr<Dispatcher> d, boost::shared_ptr<Rules> r, boost::shared_ptr<MatchStatus> ms)
                : mDispatcher(d)
                , mRules(r)
                , mMatchStatus(ms)
                , mPhysicsEngine(new BulletPhysicsEngine(Vector3(-50,-20,-50), 
                                                         Vector3(200, 100, 200), 
                                                         500, Vector3(0, -9.8, 0)))
            {
                mPhysicsEngine->subscribePhysics(mDispatcher);
                mPhysicsEngine->addStaticBoxObject(Vector3(50, 50, 50), Vector3(0, -50, 0));
                mPhysicsEngine->addDynamicSphereObject(1.0f, 5.0f, Vector3(10, 500, 10));
                mPhysicsEngine->addControllableObject(Vector3(1.0f, 2.0f, 1.0f), 80.0f, Vector3(20, 2, 20));
            }

            Physics::~Physics ( ) 
            {
                mPhysicsEngine->unsubscribePhysics(mDispatcher);
            }

            void Physics::setPause ( bool new_var ) 
            {
                mPause = new_var;
            }

            bool Physics::getPause ( ) 
            {
                return mPause;
            }

            void Physics::setNewPhysicsEvents ( PhysicsEventList new_var ) 
            {
                mNewPhysicsEvents = new_var;
            }

            PhysicsEventList Physics::getNewPhysicsEvents ( ) 
            {
                return mNewPhysicsEvents;
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

            void Physics::newClientEvent (const messages::MovePlayerControlMessage& e ) 
            {
                using namespace messages;
                addutil::Vector3 v;
                e.getTargetVector(v);
                if (!mPhysicsEngine->setObjectVelocity(e.getPlayerID(), v))
                {
                    std::cerr << "Physics::newClientEvent: Invalid player ID\n";
                }
            }
        }
    }
}

