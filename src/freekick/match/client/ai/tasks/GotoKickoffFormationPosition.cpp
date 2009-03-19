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

#include "tasks/GotoKickoffFormationPosition.h"

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
                    GotoKickoffFormationPosition::GotoKickoffFormationPosition (boost::shared_ptr<MatchStatus> ms, int id)
                        : mMatchStatus(ms),
                          mPlayerID(id)
                    {
                        boost::shared_ptr<MatchPlayer> pl = mMatchStatus->getPlayer(mPlayerID);
                        soccer::BallOwner own = mMatchStatus->getPlayerSide(mPlayerID);
                        PlayerPosition pp = pl->getPlayerPosition();
                        bool issub = pl->isSubstitute();
                        if(!issub)
                        {
                            addutil::Vector3 formationpoint;
                            try
                            {
                                const std::string& plpos = mMatchStatus->getPlayerClub(mPlayerID)->getPlayerPosition(mPlayerID);
                                formationpoint = mMatchStatus->getPlayerClub(mPlayerID)->getFormation()->getTacticAreaCenter(true, true, plpos);
                                ownformationpos.x = formationpoint.x;
                            }
                            catch(...)
                            {
                                ownformationpos.x = 0.5f;
                            }
                            switch(pp)
                            {
                                case Goalkeeper:
                                    ownformationpos.z = 0.05f;
                                    break;
                                case Defender:
                                    ownformationpos.z = 0.2f;
                                    break;
                                case Midfielder:
                                    ownformationpos.z = 0.3f;
                                    break;
                                default:
                                    ownformationpos.z = 0.45f;
                                    break;
                            }
                        }
                        else
                        {
                            ownformationpos.x = -0.1f;
                            ownformationpos.z = 0.4f;
                        }
                        if(own == Away)
                        {
                            ownformationpos.x = 1.0f - ownformationpos.x;
                            ownformationpos.z = 1.0f - ownformationpos.z;
                        }
                        float pitchw = mMatchStatus->getPitchWidth();
                        float pitchl = mMatchStatus->getPitchLength();
                        ownformationpos.x *= pitchw;
                        ownformationpos.z *= pitchl;
                    }

                    bool GotoKickoffFormationPosition::finished() const
                    {
                        const BallState bs = mMatchStatus->getBallState();
                        if(bs.bio_type != PreKickoff && bs.blocked_play == false)
                        {
                            return true;
                        }
                        return false;
                    }

                    boost::shared_ptr<messages::PlayerControlMessage> GotoKickoffFormationPosition::process()
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
