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


#ifndef RULES_H
#define RULES_H

#include <vector>

#include <boost/shared_ptr.hpp>
#include <boost/array.hpp>
#include <boost/date_time/posix_time/posix_time.hpp>

#include "addutil/Publisher.h"
#include "addutil/Reader.h"
#include "addutil/Vector3.h"

#include "MatchStatus.h"
#include "Physics.h"
#include "MatchIDs.h"
#include "Pitch.h"

#include "messages/Message.h"

namespace freekick
{
    namespace match
    {
        namespace server
        {
            typedef messages::GeneralUpdateStatusMessage RulesMessage;
            typedef std::vector<RulesMessage> RulesMessageList;
            typedef messages::GeneralUpdateScoreMessage ScoreMessage;
            typedef std::vector<ScoreMessage> ScoreMessageList;

            class Rules : public addutil::Publisher<Rules>, public addutil::Reader<Physics>
            {
            public:
                Rules (boost::shared_ptr<MatchStatus> ms, boost::shared_ptr<Physics> p);
                virtual ~Rules ();

                // Reader<Physics>
                void update(Physics* p);

                // Publisher<Rules>
                void getUpdates (RulesMessageList& pes, ScoreMessageList& scs) const;

            protected:
                // Publisher<Rules>
                void clearMessages();

            private:
                boost::shared_ptr<MatchStatus> mMatchStatus;
                boost::shared_ptr<Physics> mPhysics;
                boost::array<std::vector<int>, 2> players;
                boost::array<std::vector<int>, 2> substitutes;

                // Messages that will be published are stored here
                RulesMessageList newmessages;
                ScoreMessageList newscores;

                BallState mBallState;
                boost::shared_ptr<Pitch> mPitch;

                boost::posix_time::ptime last_update_time;
                static const unsigned long update_time_interval_ms = 1000000;

                void check_for_touched_ball(const OwnerMessageList& c, bool& new_ball_status, bool& ball_touched);
                void check_for_over_pitch(const PhysicsMessageList& l, bool& new_ball_status, GoalQuery& goal_scored);
                void check_for_bio_change(bool& new_ball_status, const bool& ball_touched);
                bool play_can_be_given_free() const;
                bool ball_due_for_throwin(const addutil::Vector3 ballvec) const;
                bool ball_over_a_goal_line(const addutil::Vector3& ballvec) const;
            };

        }
    }
}

#endif // RULES_H
