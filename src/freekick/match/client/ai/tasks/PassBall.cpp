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

#include "tasks/PassBall.h"

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
                    PassBall::PassBall (boost::shared_ptr<MatchStatus> ms, int id)
                        : mMatchStatus(ms),
                          mPlayerID(id)
                    {
                        mPlayer = mMatchStatus->getPlayer(mPlayerID);
                    }

                    bool PassBall::finished() const
                    {
                        return true;
                    }

                    boost::shared_ptr<messages::PlayerControlMessage> PassBall::process()
                    {
                        addutil::Vector3 target;
                        addutil::Vector3 ownpos = mPlayer->getPosition();
                        soccer::PlayerTarget t = mMatchStatus->getPlayerTarget(mPlayerID);
                        addutil::Vector3 tgtgoal = mMatchStatus->getGoalPosition(t);
                        addutil::Vector3 goalvec = tgtgoal - ownpos;
                        float plength = mMatchStatus->getPitchLength();

                        std::vector<addutil::Vector3> ownclub;
                        std::vector<addutil::Vector3>::const_iterator clubit;

                        float best_z    = 0.0f;
                        float own_z     = (t == UpTarget) ? plength - ownpos.z : ownpos.z;
                        addutil::Vector3 bestpass = tgtgoal;

                        mMatchStatus->getPlayerPositions(ownclub, t);
                        for(clubit = ownclub.begin(); clubit != ownclub.end(); clubit++)
                        {
                            float diff = (*clubit - ownpos).length();
                            float this_z = clubit->z;
                            if(t == UpTarget) this_z = plength - this_z;

                            if(diff < 5.0f || diff > 20.0f)
                                continue;
                            if (this_z < own_z - 10.0f)
                                continue;

                            if(this_z > best_z)
                            {
                                bestpass = *clubit;
                                best_z = this_z;
                            }
                        }
                        target = bestpass - ownpos;
                        Helpers::correctPassVector(target);
                        // std::cout << "Pass: Kick target: " << (target + ownpos) << std::endl;

                        using namespace messages;
                        return boost::shared_ptr<KickPlayerControlMessage>(new KickPlayerControlMessage(mPlayerID, target));
                    }
                }
            }
        }
    }
}
