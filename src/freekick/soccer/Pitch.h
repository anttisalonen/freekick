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
#include <boost/array.hpp>

#include "addutil/Vector3.h"

#include "Primitives.h"

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
        static const float goal_area_width = 5.5f;
        static const float goal_area_length = 5.5f;
        static const float penalty_box_area_width = 40.3f;
        static const float penalty_box_area_length = 16.5f;

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
            GoalQuery inGoalArea(const addutil::Vector3& v) const;
            GoalQuery inPenaltyBoxArea(const addutil::Vector3& v) const;

        protected:
            bool maybeInGoal(float x, float y, float z) const;
        private:
            const float width;
            const float length;
            boost::array<addutil::Vector3, 4> goal_area_points;
            boost::array<addutil::Vector3, 4> penalty_box_area_points;

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
