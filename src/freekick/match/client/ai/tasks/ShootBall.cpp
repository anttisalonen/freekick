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

#include "tasks/ShootBall.h"

namespace freekick 
{ 
    namespace match
    {
        namespace client
        {
            namespace ai_client
            {
                namespace tasks
                {
                    ShootBall::ShootBall (boost::shared_ptr<MatchStatus> ms, int id)
                        : mMatchStatus(ms),
                          mPlayerID(id)
                    {
                        mPlayer = mMatchStatus->getPlayer(mPlayerID);
                    }

                    bool ShootBall::finished() const
                    {
                        return true;
                    }

                    boost::shared_ptr<messages::PlayerControlMessage> ShootBall::process()
                    {
                        addutil::Vector3 target;
                        addutil::Vector3 ownpos = mPlayer->getPosition();
                        soccer::PlayerTarget t = mMatchStatus->getPlayerTarget(mPlayerID);
                        addutil::Vector3 tgtgoal = mMatchStatus->getGoalPosition(t);

                        target = tgtgoal - ownpos;
                        Helpers::correctShootVector(target);
                        // std::cout << "Shooting; kick target: " << (target + ownpos) << std::endl;

                        using namespace messages;
                        return boost::shared_ptr<KickPlayerControlMessage>(new KickPlayerControlMessage(mPlayerID, target));
                    }
                }
            }
        }
    }
}
