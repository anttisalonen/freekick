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

#include "Pitch.h"

namespace freekick
{
    namespace soccer
    {
        Pitch::Pitch (float w, float l)
            : width(w), length(l) 
        {
            float half_width = width / 2.0f;

            float goal_area_left = half_width - (goal_width / 2.0f) - goal_area_width;
            float goal_area_right = half_width + (goal_width / 2.0f) + goal_area_width;
            float goal_area_up = goal_area_length;
            float goal_area_down = length - goal_area_length;
            goal_area_points[0] = addutil::Vector3(goal_area_left,  0.0f, 0.0f);
            goal_area_points[1] = addutil::Vector3(goal_area_right, 0.0f, goal_area_up);
            goal_area_points[2] = addutil::Vector3(goal_area_left,  0.0f, goal_area_down);
            goal_area_points[3] = addutil::Vector3(goal_area_right, 0.0f, length);

            float penalty_box_area_left  = half_width - penalty_box_area_width / 2.0f;
            float penalty_box_area_right = half_width + penalty_box_area_width / 2.0f;
            float penalty_box_area_up    = penalty_box_area_length;
            float penalty_box_area_down  = length - penalty_box_area_length;
            penalty_box_area_points[0] = addutil::Vector3(penalty_box_area_left,  0.0f, 0.0f);
            penalty_box_area_points[1] = addutil::Vector3(penalty_box_area_right, 0.0f, penalty_box_area_up);
            penalty_box_area_points[2] = addutil::Vector3(penalty_box_area_left,  0.0f, penalty_box_area_down);
            penalty_box_area_points[3] = addutil::Vector3(penalty_box_area_right, 0.0f, length);
        }

        float Pitch::getWidth() const
        {
            return width;
        }

        float Pitch::getLength() const
        {
            return length;
        }

        bool Pitch::onPitch(float x, float y) const
        {
            if(x < 0.0f || y < 0.0f || x > width || y > length) return false;
            return true;
        }

        bool Pitch::onSide(bool top, float x, float y) const
        {
            if(!onPitch(x, y)) return false;
            bool ontopside = (y < (length / 2.0f));
            if(top) return ontopside;
            return !ontopside;
        }

        bool Pitch::inFirstGoal(const addutil::Vector3& v) const
        {
            if(!maybeInGoal(v.x, v.y, v.z)) return false;
            return (v.z < 0.0f);
        }

        bool Pitch::inSecondGoal(const addutil::Vector3& v) const
        {
            if(!maybeInGoal(v.x, v.y, v.z)) return false;
            return (v.z > length);
        }

        bool Pitch::inFirstGoal(float x, float y, float z) const
        {
            return inFirstGoal(addutil::Vector3(x, y, z));
        }

        bool Pitch::inSecondGoal(float x, float y, float z) const
        {
            return inSecondGoal(addutil::Vector3(x, y, z));
        }

        bool Pitch::maybeInGoal(float x, float y, float z) const
        {
            if(onPitch(x, z)) return false;
            if(abs((width * 0.5f) - x) > goal_width * 0.5f) return false;
            if(y < 0.0f || y > goal_height) return false;
            return true;
        }

        GoalQuery Pitch::inGoalArea(const addutil::Vector3& v) const
        {
            if(addutil::inArea(goal_area_points[0], goal_area_points[1], v)) return HomeGoal;
            if(addutil::inArea(goal_area_points[2], goal_area_points[3], v)) return AwayGoal;
            return NoGoal;
        }

        GoalQuery Pitch::inPenaltyBoxArea(const addutil::Vector3& v) const
        {
            if(addutil::inArea(penalty_box_area_points[0], penalty_box_area_points[1], v)) return HomeGoal;
            if(addutil::inArea(penalty_box_area_points[2], penalty_box_area_points[3], v)) return AwayGoal;
            return NoGoal;
        }
    }
}
