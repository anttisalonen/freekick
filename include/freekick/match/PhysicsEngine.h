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


#ifndef PHYSICSENGINE_H
#define PHYSICSENGINE_H

#include <set>
#include <map>

#include <boost/foreach.hpp>

#include "Vector3.h"
#include "DynamicEntity.h"
#include "Publisher.h"

namespace freekick
{
    namespace match
    {
        typedef unsigned int ObjectID;
        typedef addutil::DynamicEntity* EntityPtr;
        typedef std::map<ObjectID, EntityPtr> EntityPtrMap;

        class PhysicsEngine : public addutil::Publisher<PhysicsEngine>
        {
        public:
            virtual ~PhysicsEngine ( ) { }
            virtual bool addStaticBoxObject(ObjectID oid, addutil::Vector3 shape, addutil::Vector3 loc) = 0;
            virtual bool addDynamicBoxObject(ObjectID oid, addutil::Vector3 size, float mass, addutil::Vector3 loc) = 0;
            virtual bool addDynamicSphereObject(ObjectID oid, float radius, float mass, addutil::Vector3 loc) = 0;
            virtual bool addControllableObject(ObjectID oid, addutil::Vector3 size, float mass, addutil::Vector3 loc) = 0;
            virtual bool setObjectVelocity(ObjectID oid, const addutil::Vector3& vel) = 0;
            virtual bool removeObject(ObjectID oid) = 0;
            virtual bool stepWorld(float steptime) = 0;

            // TODO: split to a .cpp?
            void publishPhysics()
            {
                publish();
                updated_objects.clear();
            }

            void getUpdatedObjects(EntityPtrMap& e) const
            {
                e = updated_objects;
            }

            void addUpdatedObject(ObjectID i, EntityPtr e)   // used by motion states
            {
                updated_objects[i] = e;
            }

        private:
            EntityPtrMap curr_objects;
            EntityPtrMap updated_objects;

        };
    }
}

#endif // PHYSICSENGINE_H
