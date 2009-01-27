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

#include "BulletPhysicsEngine.h"

namespace freekick
{
    namespace match
    {
        using addutil::Vector3;

        BulletPhysicsEngine::BulletPhysicsEngine (Vector3 bottom_left, 
                                                  Vector3 top_right, 
                                                  int maxObjects, 
                                                  boost::shared_ptr<PhysicsEngineSettings> settings)
            : PhysicsEngine(settings)
            , collisionConfiguration(new btDefaultCollisionConfiguration())
            , dispatcher(new btCollisionDispatcher(collisionConfiguration.get()))
            , solver(new btSequentialImpulseConstraintSolver())
        {
            ///the maximum size of the collision world. Make sure objects stay within these boundaries
            ///Don't make the world AABB size too large, it will harm simulation quality and performance
            btVector3 worldAabbMin(bottom_left.x, bottom_left.y, bottom_left.z);
            btVector3 worldAabbMax(top_right.x, top_right.y, top_right.z);
            int       maxProxies = maxObjects;
            overlappingPairCache.reset(new btAxisSweep3(worldAabbMin,worldAabbMax,maxProxies));

            ///the default constraint solver. For parallel processing you can use a different solver (see Extras/BulletMultiThreaded)
            dynamicsWorld.reset(new btDiscreteDynamicsWorld(dispatcher.get(), overlappingPairCache.get(), solver.get(), collisionConfiguration.get()));

            dynamicsWorld->setGravity(btVector3(settings->gravity.x, settings->gravity.y, settings->gravity.z));
        }

        BulletPhysicsEngine::~BulletPhysicsEngine ()
        {
            //remove the rigidbodies from the dynamics world and delete them
            for (int i=dynamicsWorld->getNumCollisionObjects()-1; i>=0 ;i--)
            {
                btCollisionObject* obj = dynamicsWorld->getCollisionObjectArray()[i];
                btRigidBody* body = btRigidBody::upcast(obj);
                if (body && body->getMotionState())
                {
                    delete body->getMotionState();
                }
                dynamicsWorld->removeCollisionObject( obj );
                delete obj;
            }

            //delete collision shapes
            for (int j=0;j<collisionShapes.size();j++)
            {
                btCollisionShape* shape = collisionShapes[j];
                collisionShapes[j] = 0;
                delete shape;
            }

            /*
            //delete dynamics world
            delete dynamicsWorld;

            //delete solver
            delete solver;

            //delete broadphase
            delete overlappingPairCache;

            //delete dispatcher
            delete dispatcher;

            delete collisionConfiguration;
            */
        }

        bool BulletPhysicsEngine::addStaticBoxObject(ObjectID oid, Vector3 shape, Vector3 loc, float restitution)
        {
            // TODO: use only one collision shape for all same kinds of boxes
            btCollisionShape* boxshape = new btBoxShape(btVector3(btScalar(shape.x),btScalar(shape.y),btScalar(shape.z)));
            return addStaticObject(oid, boxshape, loc, restitution);
        }

        bool BulletPhysicsEngine::addStaticCylinderObject(ObjectID oid, addutil::Axis axis, float length, float radius, addutil::Vector3 loc, float restitution)
        {
            btCollisionShape* cylshape;
            if(axis == addutil::Y_Axis)
                cylshape = new btCylinderShape(btVector3(btScalar(radius),btScalar(length),btScalar(radius)));
            else if (axis == addutil::X_Axis)
                cylshape = new btCylinderShapeX(btVector3(btScalar(length),btScalar(radius),btScalar(radius)));
            else
                cylshape = new btCylinderShapeZ(btVector3(btScalar(radius),btScalar(radius),btScalar(length)));
            return addStaticObject(oid, cylshape, loc, restitution);
        }

        bool BulletPhysicsEngine::addStaticObject(ObjectID oid, btCollisionShape* colShape, addutil::Vector3 loc, float restitution)
        {
            btTransform objtransform;
            objtransform.setIdentity();
            objtransform.setOrigin(btVector3(loc.x,loc.y,loc.z));

            btScalar mass(0.);
            btVector3 localInertia(0,0,0);

            //using motionstate is recommended, it provides interpolation capabilities, and only synchronizes 'active' objects
            btDefaultMotionState* myMotionState = new btDefaultMotionState(objtransform);
            btRigidBody::btRigidBodyConstructionInfo rbInfo(mass, myMotionState, colShape, localInertia);
            rbInfo.m_restitution = restitution;
            btRigidBody* body = new btRigidBody(rbInfo);

            //add the body to the dynamics world
            return addObject(body, Collision_Static);
        }

        bool BulletPhysicsEngine::addDynamicBoxObject(ObjectID oid, Vector3 size, float mass, Vector3 loc, float restitution)
        {
            btCollisionShape* colShape = new btBoxShape(btVector3(size.x,size.y,size.z));
            try
            {
                return addDynamicObject(oid, colShape, mass, loc, restitution);
            }
            catch (...)
            {
                delete colShape;
            }
            return false;
        }

        bool BulletPhysicsEngine::addDynamicSphereObject(ObjectID oid, float radius, float mass, Vector3 loc, float restitution)
        {
            btCollisionShape* colShape = new btSphereShape(btScalar(radius));

            try
            {
                return addDynamicObject(oid, colShape, mass, loc, restitution);
            }
            catch (...)
            {
                delete colShape;
            }
            return false;
        }

        bool BulletPhysicsEngine::addControllableObject(ObjectID oid, addutil::Vector3 size, float mass, addutil::Vector3 loc)
        {
            btCollisionShape* colShape = new btBoxShape(btVector3(size.x,size.y,size.z));
            try
            {
                return addDynamicObject(oid, colShape, mass, loc, 0.0f, true);
            }
            catch (...)
            {
                delete colShape;
            }
            return false;
        }

        bool BulletPhysicsEngine::addObject(btRigidBody* b, int collgroup)
        {
            // TODO: enable collisions between ball and torso of player 
            int collmask = 0;
            switch(collgroup)
            {
                case Collision_Ball:
                    collmask = Collision_Static;
                    break;

                case Collision_Player:
                    collmask = Collision_Static | Collision_Player;
                    break;

                case Collision_Static:
                default:
                    collmask = Collision_All;
                    break;
            }
            dynamicsWorld->addRigidBody(b, collgroup, collmask);
            return true;
        }

        bool BulletPhysicsEngine::setObjectVelocity(ObjectID oid, const addutil::Vector3& vel, ObjectID oid2)
        {
            ObjectMap::iterator it;
            it = mObjectMap.find(oid);
            if (it == mObjectMap.end())
                return false;
            const btVector3 bv(vel.x, vel.y, vel.z);

            it->second->activate(true);
            it->second->setLinearVelocity(bv);

            if(oid2 != 0)
            {
                addCollidedObject(BallID, oid2, vel.length());
            }
            return true;
        }

        bool BulletPhysicsEngine::setObjectVelocityAndOrientation(ObjectID oid, const addutil::Vector3& vel, float xzangle)
        {
            ObjectMap::iterator it;
            it = mObjectMap.find(oid);
            if (it == mObjectMap.end())
                return false;

            const btVector3 bv(vel.x, vel.y, vel.z);
            const btQuaternion bq(btVector3(0.0f, 1.0f, 0.0f), xzangle);
            const btVector3 bo = it->second->getWorldTransform().getOrigin();

            it->second->activate(true);
            it->second->setLinearVelocity(bv);
            it->second->setWorldTransform(btTransform(bq, bo));
            return true;
        }

        bool BulletPhysicsEngine::setObjectPosition(ObjectID oid, const addutil::Vector3& pos)
        {
            ObjectMap::iterator it;
            it = mObjectMap.find(oid);
            if (it == mObjectMap.end())
                return false;
            const btVector3 bv(pos.x, pos.y, pos.z);
            const btQuaternion bq(1.0f, 0.0f, 0.0f, 0.0f);

            it->second->activate(true);
            it->second->setWorldTransform(btTransform(bq, bv));
            it->second->setLinearVelocity(btVector3(btScalar(0), btScalar(0), btScalar(0)));
            return true;
        }

        bool BulletPhysicsEngine::setObjectOrientation(ObjectID oid, const addutil::Quaternion& q)
        {
            const btQuaternion bq(q.w, q.x, q.y, q.z);
            return setObjectOrientation(oid, bq);
        }

        bool BulletPhysicsEngine::setObjectOrientation(ObjectID oid, float xz)
        {
            const btQuaternion bq(btVector3(0.0f, 1.0f, 0.0f), xz);
            return setObjectOrientation(oid, bq);
        }

        bool BulletPhysicsEngine::setObjectOrientation(ObjectID oid, const btQuaternion& q)
        {
            ObjectMap::iterator it;
            it = mObjectMap.find(oid);
            if (it == mObjectMap.end())
                return false;
            const btVector3 bv = it->second->getWorldTransform().getOrigin();

            it->second->activate(true);
            it->second->setWorldTransform(btTransform(q, bv));
            return true;
        }

        bool BulletPhysicsEngine::addDynamicObject(ObjectID oid, 
                                                   btCollisionShape* colShape, 
                                                   float mass, 
                                                   Vector3 loc, 
                                                   float restitution,
                                                   bool upright)
        {
            ObjectMap::iterator it = mObjectMap.find(oid);
            if (it != mObjectMap.end()) throw "BulletPhysicsEngine::addDynamicObject: Object ID already used";
            if(mass < 0.0f) throw "BulletPhysicsEngine::addDynamicObject: invalid mass";
            bool kinematic = false;

            collisionShapes.push_back(colShape);

            /// Create Dynamic Objects
            btTransform startTransform;
            startTransform.setIdentity();

            if(kinematic)
            {
                if(upright) mass = 0.0f;
            }

            btScalar        ms(mass);

            btVector3 localInertia(0,0,0);
            colShape->calculateLocalInertia(ms,localInertia);

            startTransform.setOrigin(btVector3(loc.x, loc.y, loc.z));

            //using motionstate is recommended, it provides interpolation capabilities, and only synchronizes 'active' objects
            FreekickMotionState* myMotionState = new FreekickMotionState(startTransform, this, oid);
            btRigidBody::btRigidBodyConstructionInfo rbInfo(ms, myMotionState, colShape, localInertia);
            rbInfo.m_restitution = restitution;
            btRigidBody* body = new btRigidBody(rbInfo);

            // TODO: define damping somewhere else
            body->setDamping(btScalar(0.3f), btScalar(0.5f));

            int collgroup = Collision_Ball;
            if(upright)
            {
                collgroup = Collision_Player;
                if(kinematic)
                {
                    body->setCollisionFlags(body->getCollisionFlags() | btCollisionObject::CF_KINEMATIC_OBJECT);
                }
                // body->setActivationState(DISABLE_DEACTIVATION);

                // constraint
                const btVector3 pivot(0.0f, 0.0f, 0.0f); // middle?
                btVector3 axis(0.0f, 1.0f, 0.0f);        // pointing upwards, aka Y-axis

                btHingeConstraint* hinge = new btHingeConstraint(*body, pivot, axis);
                hinge->setAngularOnly(true);

                // hinge->setLimit(0.0f, M_PI_2);
                dynamicsWorld->addConstraint(hinge);
            }

            mObjectMap[oid] = body;
            return addObject(body, collgroup);
        }

        bool BulletPhysicsEngine::getObjectPosition(ObjectID oid, addutil::Vector3& pos) const
        {
            ObjectMap::const_iterator it = mObjectMap.find(oid);
            if(it == mObjectMap.end()) return false;
            const btVector3 p(it->second->getWorldTransform().getOrigin());
            pos.set(p.x(), p.y(), p.z());
            return true;
        }

        bool BulletPhysicsEngine::getObjectOrientation(ObjectID oid, addutil::Quaternion& ori) const
        {
            ObjectMap::const_iterator it = mObjectMap.find(oid);
            if(it == mObjectMap.end()) return false;
            const btQuaternion p(it->second->getWorldTransform().getRotation());
            ori.set(p.w(), p.x(), p.y(), p.z());
            return true;
        }

        bool BulletPhysicsEngine::getObjectPositionAndOrientation(ObjectID oid, addutil::Vector3& pos, addutil::Quaternion& ori) const
        {
            ObjectMap::const_iterator it = mObjectMap.find(oid);
            if(it == mObjectMap.end()) return false;
            const btVector3 p(it->second->getWorldTransform().getOrigin());
            pos.set(p.x(), p.y(), p.z());
            const btQuaternion q(it->second->getWorldTransform().getRotation());
            ori.set(q.w(), q.x(), q.y(), q.z());
            return true;
        }

        bool BulletPhysicsEngine::removeObject(ObjectID oid)
        {
            // TODO
            return false;
        }

        bool BulletPhysicsEngine::stepWorld(float steptime)
        {
            dynamicsWorld->stepSimulation(steptime, 10); // TODO: variable maxSubSteps

            // This piece of code checks for collisions between the ball and the players.
            // TODO: add checks between (tackling) players and the rest of players
            ObjectMap::const_iterator it = mObjectMap.find(BallID);
            if(it != mObjectMap.end())
            {
                btCollisionObject* body0 = it->second;
                ObjectMap::const_iterator it2;
                for(it2 = mObjectMap.begin(); it2 != mObjectMap.end(); it2++)
                {
                    if(it2->first == it->first) continue;
                    btCollisionObject* body1 = it2->second;
                    btBroadphaseProxy* prox0 = body0->getBroadphaseHandle();
                    btBroadphaseProxy* prox1 = body1->getBroadphaseHandle();
                    btBroadphasePair* pair = dynamicsWorld->getBroadphase()->getOverlappingPairCache()->findPair(prox0, prox1);
                    if (pair)
                    {
                        btManifoldArray manifoldArray;

                        if (pair->m_algorithm) 
                            pair->m_algorithm->getAllContactManifolds(manifoldArray);

                        for (unsigned int i = 0, s = manifoldArray.size(); i < s; ++i)
                        {
                            if (manifoldArray[i]->getNumContacts() > 0)
                                addCollidedObject(it->first, it2->first, 1.0f);
                            // TODO: use actual power/collision depth
                        } 
                    }
                }

            }

            publishPhysics();

            return true;
        }

    }
}


