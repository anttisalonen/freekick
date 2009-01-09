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

#include "tasks/GotoCabins.h"

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
                    GotoCabins::GotoCabins (boost::shared_ptr<MatchStatus> ms, int id)
                        : mMatchStatus(ms),
                          mPlayerID(id)
                    {
                        soccer::BallOwner own = mMatchStatus->getPlayerSide(mPlayerID);
                        ownformationpos.x = -0.1f;
                        ownformationpos.z = 0.4f;
                        if(own == Away)
                        {
                            ownformationpos.z = 1.0f - ownformationpos.z;
                        }
                        float pitchw = mMatchStatus->getMatchData()->getStadium()->getPitch()->getWidth();
                        float pitchl = mMatchStatus->getMatchData()->getStadium()->getPitch()->getLength();
                        ownformationpos.x *= pitchw;
                        ownformationpos.z *= pitchl;
                    }

                    bool GotoCabins::finished() const
                    {
                        const BallState bs = mMatchStatus->getBallState();
                        if(bs.bio_type != HalfFullTime)
                        {
                            return true;
                        }
                        return false;
                    }

                    boost::shared_ptr<messages::PlayerControlMessage> GotoCabins::process()
                    {
                        boost::shared_ptr<MatchPlayer> pl = mMatchStatus->getPlayer(mPlayerID);
                        addutil::Vector3 ownpos = pl->getPosition();
                        addutil::Vector3 gotovec = ownformationpos - ownpos;
                        if(gotovec.length() < 1.0f) gotovec.reset();
                        gotovec.y = 0.0f;

                        using namespace messages;
                        return boost::shared_ptr<MovePlayerControlMessage>(new MovePlayerControlMessage(mPlayerID, gotovec));
                    }
                }
            }
        }
    }
}
