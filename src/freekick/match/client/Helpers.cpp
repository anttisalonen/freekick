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

#include "Helpers.h"

namespace freekick 
{ 
    namespace match
    {
        namespace client
        {
            Helpers::Helpers (boost::shared_ptr<MatchStatus> ms, int id)
                : mMatchStatus(ms),
                  mPlayerID(id),
                  mPlayer(mMatchStatus->getPlayer(mPlayerID)),
                  b(mMatchStatus->getPlayerSide(mPlayerID)),
                  nearestown(mMatchStatus->nearestPlayerFromClubToBall(b)),
                  bss(mMatchStatus->getBallState()),
                  allowed_to_kick(mMatchStatus->playerAllowedToKick(mPlayerID) || (bss.blocked_play && bss.owner == b)),
                  bio(bss.bio_type),
                  iskickoff((bio == Kickoff)),
                  blocked(bss.blocked_play),
                  own(bss.owner),
                  isnearestplayer(nearestown.get<0>() == mPlayerID),
                  startplay((iskickoff && !blocked && isnearestplayer && own == b)),
                  abletokick(isnearestplayer && nearestown.get<1>() < 1.5f), // TODO: read max. kicking distance from somewhere else
                  issub(mPlayer->isSubstitute()),
                  nearestother(mMatchStatus->nearestPlayerFromClubToBall(other(b))),
                  amnearerthanthey(nearestown.get<1>() < nearestother.get<1>()),
                  ballinourgoalarea(ownersGoal(b, mMatchStatus->ballInGoalArea())),
                  holdingtheball(mMatchStatus->holdingBall() == mPlayerID),
                  ballisheld(mMatchStatus->holdingBall() != 0)
            {
                // TODO: replace with lazy initialization
            }

            void Helpers::correctPassVector(addutil::Vector3& target)
            {
                if (target.length() > 20.0f)
                {
                    target *= 1.5f;
                }
                target.y = target.length() * 0.1f;
                target.x *= 1.1f;
                target.z *= 1.1f;
            }

            void Helpers::correctShootVector(addutil::Vector3& target)
            {
                target.y = target.length() * 0.3f;
                target.normalize();
                target *= 30.0f;
            }
        }
    }
}
