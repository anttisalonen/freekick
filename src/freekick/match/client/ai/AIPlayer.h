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


#ifndef AIPLAYER_H
#define AIPLAYER_H

#include <string>

#include <boost/shared_ptr.hpp>
#include <boost/foreach.hpp>

#include "addutil/Square.h"
#include "addutil/Vector3.h"
#include "addutil/General.h"
#include "addutil/Steering.h"
#include "addutil/Exception.h"

#include "messages/PlayerControlMessage.h"
#include "messages/MovePlayerControlMessage.h"
#include "messages/KickPlayerControlMessage.h"
#include "messages/HoldPlayerControlMessage.h"

#include "MatchStatus.h"
#include "MatchPlayer.h"
#include "Club.h"
#include "BallState.h"
#include "Constants.h"

#include "ClubAI.h"
#include "AIConfig.h"
#include "Helpers.h"

namespace freekick
{
    namespace match
    {
        namespace client
        {
            namespace ai_client
            {
                typedef boost::shared_ptr<messages::PlayerControlMessage> CtrlMsg;
                typedef boost::tuple<float, addutil::Vector3> optimal_kick;
                class AIPlayer
                {
                public:
                    AIPlayer(boost::shared_ptr<MatchStatus> ms, 
                            boost::shared_ptr<MatchPlayer> mp, 
                            boost::shared_ptr<ClubAI> clubai, 
                            bool active = true);
                    virtual ~AIPlayer();
                    boost::shared_ptr<messages::PlayerControlMessage> act();
                    int getID() const;

                protected:
                    void set_own_kickoff_pos();
                    addutil::Vector3 cabins_pos() const;
                    CtrlMsg runTo(const addutil::Vector3& tgt, float vel = 10.0f) const;
                    CtrlMsg seekTo(const addutil::Vector3& tgt, float vel = 10.0f) const;
                    CtrlMsg think();

                    CtrlMsg idle();
                    CtrlMsg kickoffpos();
                    CtrlMsg goalkeeper();
                    CtrlMsg gkgetball();
                    CtrlMsg kickball();
                    CtrlMsg fetchball();
                    CtrlMsg idleinformation();
                    CtrlMsg defensive();
                    CtrlMsg support();
                    CtrlMsg inownarea();
                    CtrlMsg gotocabins();
                    optimal_kick getOptimalPass() const;
                    optimal_kick getOptimalShot() const;
                    optimal_kick getOptimalDribble() const;
                    optimal_kick getOptimalLongBall() const;
                    void maybePrepareForKick(addutil::Vector3& kickvec) const;
                private:
                    boost::shared_ptr<MatchStatus> mMatchStatus;
                    boost::shared_ptr<MatchPlayer> mPlayer;
                    boost::shared_ptr<ClubAI> mClubAI;
                    int mPlayerID;
                    bool mActive;
                    addutil::Vector3 ownkickoffpos;
                    addutil::Vector3 ownpos;
                    addutil::Vector3 futpos;
                    float futpos_dist;
                    float disttoball;
                    float dangerlevel;

                    bool allowed_to_kick;
                    bool isnearestplayer;
                    bool abletokick;
                    bool issub;
                    std::string plpos;
                    bool ballinmyarea;
                    bool holdingtheball;
                    bool startplay;
                    bool mGoalie;
                    addutil::Vector3 goalvec;
                    addutil::Square formsq;

                    static const float max_velocity = 10.0f; // TODO: does this belong here?
                    static const float max_danger_dist = 20.0f;
                    static const float min_walk_vel = 1.5f;
                    static const float gen_jog_vel = 3.0f;
                };
            }
        }
    }
}

#endif
