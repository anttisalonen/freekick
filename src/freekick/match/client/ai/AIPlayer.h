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

#include <vector>
#include <set>

#include <boost/shared_ptr.hpp>
#include <boost/foreach.hpp>

#include "addutil/DynamicEntity.h"

#include "messages/PlayerControlMessage.h"
#include "messages/MovePlayerControlMessage.h"
#include "messages/KickPlayerControlMessage.h"
#include "messages/HoldPlayerControlMessage.h"

#include "MatchStatus.h"
#include "Club.h"
#include "BallState.h"
#include "tasks/Task.h"
#include "tasks/Soccer.h"

namespace freekick
{
    namespace match
    {
        namespace client
        {
            namespace ai_client
            {
                class AIPlayer
                {
                public:
                    AIPlayer(boost::shared_ptr<MatchStatus> ms, int id, bool active = true);
                    virtual ~AIPlayer();
                    boost::shared_ptr<messages::PlayerControlMessage> act();
                    int getID() const;

                private:
                    boost::shared_ptr<MatchStatus> mMatchStatus;
                    int mPlayerID;
                    boost::shared_ptr<MatchPlayer> mPlayer;
                    boost::shared_ptr<Club> mClub;
                    boost::shared_ptr<Club> mOpponentClub;
                    std::vector<boost::shared_ptr<MatchPlayer> > mTeammates;
                    std::vector<boost::shared_ptr<MatchPlayer> > mOpponents;
                    bool mActive;
                    boost::shared_ptr<tasks::Task> mTask;

                    void fillPlayers(std::vector<boost::shared_ptr<MatchPlayer> >& pl, const std::set<int>& plids);
                };
            }
        }
    }
}

#endif
