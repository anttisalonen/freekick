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
**************************************************************************/


#ifndef FREEKICK_PHYSICSREADER_H
#define FREEKICK_PHYSICSREADER_H

#include <map>

#include <boost/shared_ptr.hpp>

#include "DynamicEntity.h"

/**
 * class PhysicsReader
 */

namespace freekick
{
    namespace match
    {
        typedef unsigned int ObjectID;
        typedef addutil::DynamicEntity* EntityPtr;
        typedef std::map<ObjectID, EntityPtr> EntityPtrMap;

        class PhysicsReader
        {
        public:
            virtual ~PhysicsReader() { }
            virtual void updatePhysics(EntityPtrMap m) = 0;
        };

        typedef boost::shared_ptr<PhysicsReader> PhysicsReaderPtr;
    }
}

#endif // FREEKICK_PHYSICSREADER_H
