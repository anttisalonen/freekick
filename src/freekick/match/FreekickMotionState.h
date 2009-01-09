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


#ifndef FREEKICKMOTIONSTATE_H
#define FREEKICKMOTIONSTATE_H

#include "btBulletDynamicsCommon.h"

#include "addutil/DynamicEntity.h"

#include "PhysicsEngine.h"

namespace freekick
{
    namespace match
    {
        class FreekickMotionState : public btMotionState, public addutil::DynamicEntity
        {
        public:
            FreekickMotionState(const btTransform& initialpos, PhysicsEngine* pe, const ObjectID id);
            virtual ~FreekickMotionState();
            virtual void getWorldTransform(btTransform &worldTrans) const;
            virtual void setWorldTransform(const btTransform &worldTrans);
            const int getID() const;

        private:
            PhysicsEngine* mPhysicsEngine;
            btTransform mPos1;
            ObjectID mId; 
        };
    }
}

#endif
