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

#include <boost/foreach.hpp>

#include "Vector3.h"

#include "PhysicsReader.h"

namespace freekick
{
    namespace match
    {
        class PhysicsEngine
        {
        public:
            virtual ~PhysicsEngine ( ) { }
            virtual ObjectID addStaticBoxObject(addutil::Vector3 shape, addutil::Vector3 loc) = 0;
            virtual ObjectID addDynamicBoxObject(addutil::Vector3 size, float mass, addutil::Vector3 loc) = 0;
            virtual ObjectID addDynamicSphereObject(float radius, float mass, addutil::Vector3 loc) = 0;
            virtual ObjectID addControllableObject(addutil::Vector3 size, float mass, addutil::Vector3 loc) = 0;
            virtual bool setObjectVelocity(ObjectID oid, const addutil::Vector3& vel) = 0;
            virtual bool removeObject(ObjectID oid) = 0;
            virtual bool stepWorld(float steptime) = 0;

            // TODO: split to a .cpp?
            void subscribePhysics(PhysicsReaderPtr r)
            {
                readers.insert(r);
            }

            void unsubscribePhysics(PhysicsReaderPtr r)
            {
                readers.erase(r);
            }

            void publishPhysics()
            {
                BOOST_FOREACH(PhysicsReaderPtr r, readers)
                {
                    r->updatePhysics(updated_objects);
                }
                updated_objects.clear();
            }

            void addUpdatedObject(ObjectID i, EntityPtr e)
            {
                updated_objects[i] = e;
            }

        protected:
            void getUpdatedObjects(EntityPtrMap& e)
            {
                e = updated_objects;
            }

        private:
            std::set<PhysicsReaderPtr> readers;
            EntityPtrMap curr_objects;
            EntityPtrMap updated_objects;

        };
    }
}

#endif // PHYSICSENGINE_H
