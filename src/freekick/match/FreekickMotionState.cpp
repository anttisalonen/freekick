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
**************************************************************************/

#include "FreekickMotionState.h"

namespace freekick
{
    namespace match
    {
        FreekickMotionState::FreekickMotionState(const btTransform& initialpos, PhysicsEngine* pe, const ObjectID id)
            : mPhysicsEngine(pe)
            , mPos1(initialpos)
            , mId(id)
        {
        }

        FreekickMotionState::~FreekickMotionState() 
        {
        }

        void FreekickMotionState::getWorldTransform(btTransform &worldTrans) const 
        {
            worldTrans = mPos1;
        }

        void FreekickMotionState::setWorldTransform(const btTransform &worldTrans) 
        {
            mPos1 = worldTrans;
            btQuaternion rot = worldTrans.getRotation();
            btVector3 pos = worldTrans.getOrigin();
            setOrientation(rot.w(), rot.x(), rot.y(), rot.z());
            setPosition(pos.x(), pos.y(), pos.z());
            mPhysicsEngine->addUpdatedObject(mId, this);
        }

        const int FreekickMotionState::getID() const
        {
            return mId;
        }
    }
}
