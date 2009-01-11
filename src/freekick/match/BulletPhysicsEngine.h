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


#ifndef BULLETPHYSICSENGINE_H
#define BULLETPHYSICSENGINE_H

#include <map>

#include <boost/shared_ptr.hpp>

#include "btBulletDynamicsCommon.h"

#include "MatchIDs.h"

#include "PhysicsEngine.h"
#include "FreekickMotionState.h"

namespace freekick
{
    namespace match
    {
        typedef std::map<ObjectID, btRigidBody*> ObjectMap;

        class BulletPhysicsEngine : public PhysicsEngine
        {
        public:
            BulletPhysicsEngine (addutil::Vector3 bottom_left,
                                 addutil::Vector3 top_right,
                                 int maxObjects,
                                 boost::shared_ptr<PhysicsEngineSettings> settings);
            ~BulletPhysicsEngine ();
            bool addStaticBoxObject(ObjectID oid, addutil::Vector3 shape, addutil::Vector3 loc);
            bool addDynamicBoxObject(ObjectID oid, addutil::Vector3 size, float mass, addutil::Vector3 loc, float restitution);
            bool addDynamicSphereObject(ObjectID oid, float radius, float mass, addutil::Vector3 loc, float restitution);
            bool addControllableObject(ObjectID oid, addutil::Vector3 size, float mass, addutil::Vector3 loc);
            bool setObjectVelocity(ObjectID oid, const addutil::Vector3& vel, ObjectID oid2 = 0);
            bool setObjectVelocityAndOrientation(ObjectID oid, const addutil::Vector3& vel, float xzangle);
            bool setObjectPosition(ObjectID oid, const addutil::Vector3& pos);
            bool setObjectOrientation(ObjectID oid, const addutil::Quaternion& q);
            bool setObjectOrientation(ObjectID oid, float xz);
            bool removeObject(ObjectID oid);
            bool stepWorld(float steptime);

        private:
            const static int Collision_Static = 0x01;
            const static int Collision_Ball   = 0x02;
            const static int Collision_Player = 0x04;
            const static int Collision_All    = 0xFF;

        protected:
            bool addDynamicObject(ObjectID oid, btCollisionShape* colShape, float mass, addutil::Vector3 loc, float restitution, bool upright = false);
            bool addObject(btRigidBody* b, int collgroup = Collision_Static);
            bool setObjectOrientation(ObjectID oid, const btQuaternion& q);

        private:
            // General Bullet World stuff
            ///collision configuration contains default setup for memory, collision setup. Advanced users can create their own configuration.
            boost::shared_ptr<btDefaultCollisionConfiguration> collisionConfiguration;

            ///use the default collision dispatcher. For parallel processing you can use a diffent dispatcher (see Extras/BulletMultiThreaded)
            boost::shared_ptr<btCollisionDispatcher> dispatcher;

            boost::shared_ptr<btAxisSweep3> overlappingPairCache;

            ///the default constraint solver. For parallel processing you can use a different solver (see Extras/BulletMultiThreaded)
            boost::shared_ptr<btSequentialImpulseConstraintSolver> solver;

            boost::shared_ptr<btDiscreteDynamicsWorld> dynamicsWorld;

            // Per-Object-stuff

            //keep track of the shapes, we release memory at exit.
            //make sure to re-use collision shapes among rigid bodies whenever possible!
            btAlignedObjectArray<btCollisionShape*> collisionShapes;

            ObjectMap mObjectMap;
        };
    }
}

#endif // BULLETPHYSICSENGINE_H
