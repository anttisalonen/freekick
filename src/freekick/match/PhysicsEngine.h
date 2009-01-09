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


#ifndef PHYSICSENGINE_H
#define PHYSICSENGINE_H

#include <set>
#include <vector>
#include <map>

#include <boost/shared_ptr.hpp>
#include <boost/tuple/tuple.hpp>

#include "addutil/Vector3.h"
#include "addutil/DynamicEntity.h"
#include "addutil/Publisher.h"

#include "PhysicsEngineSettings.h"

namespace freekick
{
    namespace match
    {
        typedef long int ObjectID;
        typedef addutil::DynamicEntity* EntityPtr;
        typedef std::map<ObjectID, EntityPtr> EntityPtrMap;
        typedef boost::tuple<ObjectID, ObjectID, float> Collision;   // Object IDs of the collided objects + collision power
        typedef std::vector<Collision> CollisionList;

        class PhysicsEngine : public addutil::Publisher<PhysicsEngine>
        {
        public:
            virtual ~PhysicsEngine ( ) { }
            virtual bool addStaticBoxObject(ObjectID oid, addutil::Vector3 shape, addutil::Vector3 loc) = 0;
            virtual bool addDynamicBoxObject(ObjectID oid, addutil::Vector3 size, float mass, addutil::Vector3 loc, float restitution) = 0;
            virtual bool addDynamicSphereObject(ObjectID oid, float radius, float mass, addutil::Vector3 loc, float restitution) = 0;
            virtual bool addControllableObject(ObjectID oid, addutil::Vector3 size, float mass, addutil::Vector3 loc) = 0;
            virtual bool setObjectVelocity(ObjectID oid, const addutil::Vector3& vel, ObjectID oid2 = 0) = 0;
            virtual bool setObjectPosition(ObjectID oid, const addutil::Vector3& pos) = 0;
            virtual bool removeObject(ObjectID oid) = 0;
            virtual bool stepWorld(float steptime) = 0;

            const boost::shared_ptr<PhysicsEngineSettings>& getSettings() const
            {
                return settings;
            }

            // TODO: split to a .cpp?
            void publishPhysics()
            {
                publish();
                updated_objects.clear();
                collided_objects.clear();
            }

            void getUpdatedObjects(EntityPtrMap& e) const
            {
                e = updated_objects;
            }

            void getCollidedObjects(CollisionList& l) const
            {
                l = collided_objects;
            }

            void addUpdatedObject(ObjectID i, EntityPtr e)   // used by motion states
            {
                updated_objects[i] = e;
            }

        protected:
            void addCollidedObject(ObjectID i1, ObjectID i2, float p)   // used by derived classes
            {
                if(i1 == i2) return;
                if(i1 < i2)
                    collided_objects.push_back(Collision(i1, i2, p));
                else
                    collided_objects.push_back(Collision(i2, i1, p));
            }

        protected:
            PhysicsEngine(boost::shared_ptr<PhysicsEngineSettings> s)
                : settings(s) { }

        private:
            EntityPtrMap curr_objects;
            EntityPtrMap updated_objects;
            CollisionList collided_objects;
            
            boost::shared_ptr<PhysicsEngineSettings> settings;
        };
    }
}

#endif // PHYSICSENGINE_H
