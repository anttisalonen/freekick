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


#ifndef PHYSICSENGINESETTINGS_H
#define PHYSICSENGINESETTINGS_H

#include "Vector3.h"

namespace freekick
{
    namespace match
    {
        class PhysicsEngineSettings
        {
        public:
            PhysicsEngineSettings(addutil::Vector3 _gravity, addutil::Vector3 _wind = addutil::Vector3(), float _air_viscosity = 0.0f, float _restitution = 0.0f);
            virtual ~PhysicsEngineSettings() { }

            addutil::Vector3 gravity;
            addutil::Vector3 wind;
            float air_viscosity;
            float restitution_coefficient;
        };
    }
}

#endif
