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
            Physics::Physics (boost::shared_ptr<MatchStatus> ms, boost::shared_ptr<InputMonitor> im)
                : mMatchStatus(ms),
                  mPhysicsEngine(new BulletPhysicsEngine(Vector3(-200,-100,-200), 
                                                         Vector3( 200, 100, 200), 
                                                         500,
                                                         boost::shared_ptr<PhysicsEngineSettings>(new PhysicsEngineSettings(Vector3(0, -9.8, 0))))),
                  mInputMonitor(im)
            {
                mPhysicsEngine->subscribe(*this);

                float pwidth = mMatchStatus->getMatchData()->getStadium()->getPitch()->getWidth();
                float plength = mMatchStatus->getMatchData()->getStadium()->getPitch()->getLength();
                float middle_x = pwidth / 2.0f;
                float middle_z = plength / 2.0f;
                mPhysicsEngine->addStaticBoxObject(PitchID, Vector3(pwidth * 2.0f, 50, plength * 2.0f), Vector3(0, -50, 0));
                mPhysicsEngine->addDynamicSphereObject(BallID, 1.0f, 5.0f, Vector3(middle_x, 10, middle_z), 0.7f);
                std::vector<int> hplayers;
                std::vector<int> aplayers;
                boost::shared_ptr<Club> club1 = mMatchStatus->getMatchData()->getHomeClub();
                boost::shared_ptr<Club> club2 = mMatchStatus->getMatchData()->getAwayClub();
                club1->getPlayerIDs(hplayers);
                club2->getPlayerIDs(aplayers);
                BOOST_FOREACH(int idnum, hplayers)
                {
                    Vector3 loc(idnum % (int)pwidth + 1, idnum % 2 + 2, (idnum * 2) % (int)plength + 1);
                    mPhysicsEngine->addControllableObject(idnum, Vector3(1.0f, collision_box_height, 1.0f), 80.0f, loc);
                }
                BOOST_FOREACH(int idnum, aplayers)
                {
                    Vector3 loc(idnum % (int)pwidth + 1, idnum % 2 + 4, (idnum * 2) % (int)plength + 1);
                    mPhysicsEngine->addControllableObject(idnum, Vector3(1.0f, collision_box_height, 1.0f), 80.0f, loc);
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
                bool successful = true;

                std::map<int, addutil::Vector3>::iterator it;
                std::map<int, addutil::Vector3> velocityMap;
                while(successful)
                {
                    ptime before_time(microsec_clock::local_time());

                    successful = mPhysicsEngine->stepWorld(1.0f/60.0f);
                    velocityMap = mInputMonitor->getVelocities();
                    for(it = velocityMap.begin(); it != velocityMap.end(); it++)
                    {
                        if (!mPhysicsEngine->setObjectVelocity((*it).first, (*it).second))
                        {
                            std::cerr << "Physics::run: Invalid player ID\n";
                            velocityMap.erase(it);
                            it = velocityMap.begin();
                        }
                    }
                    mInputMonitor->interpolate(sleep_time);

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

            void Physics::update(PhysicsEngine* e)
            {
                EntityPtrMap m;
                e->getUpdatedObjects(m);
                typedef std::pair<ObjectID, EntityPtr> pair_en;
                BOOST_FOREACH(pair_en p, m)
                {
                    const addutil::Vector3 pos = p.second->getPosition();
                    addutil::Vector3 pos_corrected = pos;
                    if(p.first > 0)
                        pos_corrected.y -= collision_box_height;
                    const addutil::Quaternion orien = p.second->getOrientation();
                    messages::ConstantUpdateMessage c(p.first, 0, pos_corrected, orien);
                    newmessages.push_back(c);
                }
                publish();
                mMatchStatus->update(newmessages);
                clearMessages();
            }

            void Physics::clearMessages()
            {
                newmessages.clear();
                newcollisions.clear();
            }

            void Physics::getUpdates(PhysicsMessageList& l) const
            {
                l = newmessages;
            }

            void Physics::getUpdates(PhysicsMessageList& l, CollisionList& c) const
            {
                getUpdates(l);
                c = newcollisions;
            }
        }
    }
}

