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

#include "tasks/IdleInFormation.h"

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
                    IdleInFormation::IdleInFormation (const boost::shared_ptr<MatchStatus>& ms, int id)
                        : mMatchStatus(ms),
                          mPlayerID(id)
                    {
                        mPlayer = mMatchStatus->getPlayer(mPlayerID);
                    }

                    bool IdleInFormation::finished() const
                    {
                        // return true;
                        // float len = (mPlayer->getPosition() - mTarget).length();
                        // if(len < 5.0f) return true;
                        return false;
                    }

                    boost::shared_ptr<messages::PlayerControlMessage> IdleInFormation::process()
                    {
                        soccer::PlayerTarget b = mMatchStatus->getPlayerTarget(mPlayerID);
                        const boost::shared_ptr<Formation> f = mMatchStatus->getPlayerClub(mPlayerID)->getFormation();
                        PlayerPosition pp = mPlayer->getPlayerPosition();
                        addutil::Vector3 formationpoint = f->getPlayerAreaCenter(mPlayerID);
                        addutil::Vector3 movedFormationpoint(formationpoint);
                        float plength = mMatchStatus->getPitchLength();
                        float pwidth = mMatchStatus->getPitchWidth();
                        float bheight = mMatchStatus->getBall()->getPosition().z / plength;
                        float bwidth = mMatchStatus->getBall()->getPosition().x / pwidth;
                        if(b == UpTarget)
                        {
                            bheight = 1.0f - bheight;
                        }
                        // 0.0f: own goal; 1.0f: opponent's goal

                        float target_z_modifier = (1.0f - formationpoint.z) * (bheight - formationpoint.z) * (pp == Goalkeeper ? 0.04f : 0.2f);
                        movedFormationpoint.z += target_z_modifier;

                        float target_x_modifier = (bwidth - 0.5f) * 0.2f;
                        if(b == UpTarget)
                        {
                            movedFormationpoint.x -= target_x_modifier;
                        }
                        else
                        {
                            movedFormationpoint.x += target_x_modifier;
                        }
                        addutil::general::clamp(movedFormationpoint.x, 0.0f, 1.0f);

                        if((movedFormationpoint - mPlayer->getPosition()).length() > 5.0f)
                        {
                            mTarget = movedFormationpoint;
                        }
                        else
                        {
                            mTarget = formationpoint;
                        }

                        if(b == UpTarget)
                        {
                            mTarget.z = 1.0f - mTarget.z;
                            mTarget.x = 1.0f - mTarget.x;
                        }

                        mTarget.x *= pwidth;
                        mTarget.z *= plength;

                        addutil::Vector3 gotovec = mTarget - mPlayer->getPosition();
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
