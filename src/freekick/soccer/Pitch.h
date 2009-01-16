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
  This file was generated on So Okt 26 2008 at 12:09:20
**************************************************************************/


#ifndef PITCH_H
#define PITCH_H

#include <string>

#include <boost/serialization/serialization.hpp>

#include "addutil/Vector3.h"

/**
 * class Pitch
 */

namespace freekick
{
    namespace soccer
    {
        // TODO: move such magic constants to a separate .h
        static const float goal_width = 7.32f;
        static const float goal_height = 2.44f;

        class Pitch
        {
        public:
            Pitch (float w, float l );
            float getWidth() const;
            float getLength() const;
            bool onPitch(float x, float y) const;
            bool onSide(bool top, float x, float y) const;
            bool inFirstGoal(const addutil::Vector3& v) const;
            bool inSecondGoal(const addutil::Vector3& v) const;
            bool inFirstGoal(float x, float y, float z) const;
            bool inSecondGoal(float x, float y, float z) const;

        protected:
            bool maybeInGoal(float x, float y, float z) const;
        private:
            const float width;
            const float length;

            friend class boost::serialization::access;
            template<class Archive>
                void serialize(Archive & ar, const unsigned int version)
            {
                ar & width;
                ar & length;
            }
        };
    }
}

#endif // PITCH_H
