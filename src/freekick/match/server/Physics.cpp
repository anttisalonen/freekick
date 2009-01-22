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

                // pitch
                float pwidth = mMatchStatus->getMatchData()->getStadium()->getPitch()->getWidth();
                float plength = mMatchStatus->getMatchData()->getStadium()->getPitch()->getLength();
                float ptopz = 0.0f;
                float pbottomz = plength + ptopz;
                float middle_x = pwidth / 2.0f;
                float middle_z = plength / 2.0f;
                mPhysicsEngine->addStaticBoxObject(PitchID, Vector3(pwidth * 2.0f, 50, plength * 2.0f), Vector3(0, -50, 0), 0.7f);

                // goals
                float gwidth = soccer::goal_width;
                float gheight = soccer::goal_height;
                float gpradius = 0.05f;
                float gprestitution = 0.9f;
                mPhysicsEngine->addStaticCylinderObject(FirstGoalID, addutil::X_Axis, gwidth, 
                                                        gpradius, Vector3(middle_x, gheight, ptopz), gprestitution);
                mPhysicsEngine->addStaticCylinderObject(FirstGoalID - 1, addutil::Y_Axis, gheight, 
                                                        gpradius, Vector3(middle_x - (gwidth * 0.5f), gheight / 2.0f, ptopz), gprestitution);
                mPhysicsEngine->addStaticCylinderObject(FirstGoalID - 2, addutil::Y_Axis, gheight , 
                                                        gpradius, Vector3(middle_x + (gwidth * 0.5f), gheight / 2.0f, ptopz), gprestitution);

                mPhysicsEngine->addStaticCylinderObject(SecondGoalID, addutil::X_Axis, gwidth, 
                                                        gpradius, Vector3(middle_x, gheight, pbottomz), gprestitution);
                mPhysicsEngine->addStaticCylinderObject(SecondGoalID - 1, addutil::Y_Axis, gheight, 
                                                        gpradius, Vector3(middle_x - (gwidth * 0.5f), gheight / 2.0f, pbottomz), gprestitution);
                mPhysicsEngine->addStaticCylinderObject(SecondGoalID - 2, addutil::Y_Axis, gheight, 
                                                        gpradius, Vector3(middle_x + (gwidth * 0.5f), gheight / 2.0f, pbottomz), gprestitution);

                // ball
                mPhysicsEngine->addDynamicSphereObject(BallID, ball_radius, 0.43f, Vector3(middle_x, 10, middle_z), 0.95f);

                // players
                std::vector<int> hplayers;
                std::vector<int> aplayers;
                boost::shared_ptr<Club> club1 = mMatchStatus->getMatchData()->getHomeClub();
                boost::shared_ptr<Club> club2 = mMatchStatus->getMatchData()->getAwayClub();
                club1->getPlayerIDs(hplayers);
                club2->getPlayerIDs(aplayers);
                BOOST_FOREACH(int idnum, hplayers)
                {
                    Vector3 loc(idnum % (int)pwidth + 1, idnum % 2 + 2, (idnum * 2) % (int)plength + 1);
                    mPhysicsEngine->addControllableObject(idnum, Vector3(0.6f, collision_box_height, 0.4f), 80.0f, loc);
                }
                BOOST_FOREACH(int idnum, aplayers)
                {
                    Vector3 loc(idnum % (int)pwidth + 1, idnum % 2 + 4, (idnum * 2) % (int)plength + 1);
                    mPhysicsEngine->addControllableObject(idnum, Vector3(0.6f, collision_box_height, 0.4f), 80.0f, loc);
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
                float fps = 30.0f;
                float frametime = 1.0f / fps;
                unsigned long sleep_time = frametime * 1000000;
                bool successful = true;
                long time_left = 0;

                std::map<int, addutil::Vector3>::iterator it;
                boost::shared_ptr<std::map<int, addutil::Vector3> > velocityMap;
                boost::shared_ptr<std::map<int, addutil::Vector3> > kickMap;
                std::map<int, addutil::Vector3>::iterator kickit;
                while(successful && mMatchStatus->continuing())
                {
                    ptime before_time(microsec_clock::local_time());

                    successful = mPhysicsEngine->stepWorld(frametime);
                    const BallState bs = mMatchStatus->getBallState();
                    if(bs.bio_type == PreKickoff || (bs.bio_type == Kickoff && bs.blocked_play))
                    {
                        addutil::Vector3 p = mMatchStatus->getCentreSpot();
                        p.y = ball_radius;
                        mPhysicsEngine->setObjectPosition(BallID, p);
                    }
                    else if(bs.blocked_play && 
                            (bs.bio_type != BallIn &&
                             bs.bio_type != HalfFullTime))
                    {
                        addutil::Vector3 p = bs.restart_point;
                        p.y = ball_radius;
                        mPhysicsEngine->setObjectPosition(BallID, p);
                    }

                    kickMap = mInputMonitor->getKicks();
                    kickit = kickMap->begin();
                    while(kickit != kickMap->end())
                    {
                        if (!mPhysicsEngine->setObjectVelocity(BallID, kickit->second, kickit->first))
                        {
                            std::cerr << "Physics::run: Invalid kick (no ball?)\n";
                        }
                        kickMap->erase(kickit);
                        kickit = kickMap->begin();
                    }

                    velocityMap = mInputMonitor->getVelocities();
                    for(it = velocityMap->begin(); it != velocityMap->end(); it++)
                    {
                        float angle = it->second.XZAngle();
                        if (!mPhysicsEngine->setObjectVelocityAndOrientation((*it).first, (*it).second, angle))
                        {
                            std::cerr << "Physics::run: Invalid player ID\n";
                            velocityMap->erase(it);
                            it = velocityMap->begin();
                        }
                    }
                    mInputMonitor->interpolate(sleep_time);

                    ptime after_time(microsec_clock::local_time());

                    time_period diff_time(before_time, after_time);
                    time_duration diff_dur = diff_time.length();
                    long us_diff = diff_dur.total_microseconds();
                    time_left = sleep_time - us_diff;
                    if(time_left > 0)
                        boost::this_thread::sleep(microseconds(time_left));
                    else
                    {
/*
                        if(fps > 10)
                        {
                            fps--;
                            frametime = 1.0f / fps;
                            sleep_time = frametime * 1000000;
                        }
*/
                        std::cerr << "Physics::run: overrun by " << -time_left << " microseconds; fps: " << fps << std::endl;
                    }
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

                CollisionList l;
                e->getCollidedObjects(l);
                BOOST_FOREACH(Collision c, l)
                {
                    // Note: if c.get<1>() == BallID (collided object ID < BallID),
                    // then the collision won't be published.
                    if(c.get<0>() != BallID)
                        continue;
                    messages::GeneralUpdateOwnerMessage u(c.get<1>());
                    newowners.push_back(u);
                }

                publish();
                mMatchStatus->update(newmessages);
                // TODO: save owner in matchstatus?
                clearMessages();
            }

            void Physics::clearMessages()
            {
                newmessages.clear();
                newowners.clear();
            }

            void Physics::getUpdates(PhysicsMessageList& l, OwnerMessageList& c) const
            {
                l = newmessages;
                c = newowners;
            }
        }
    }
}

