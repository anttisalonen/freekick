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


#ifndef FREEKICK_MATCH_MESSAGES_HEADPLAYERCONTROLMESSAGE_H
#define FREEKICK_MATCH_MESSAGES_HEADPLAYERCONTROLMESSAGE_H

#include "PlayerControlMessage.h"

namespace freekick
{
    namespace match
    {
        namespace messages
        {
            class HeadPlayerControlMessage : public PlayerControlMessage
            {
            public:
                HeadPlayerControlMessage(PlayerID plid, addutil::Vector3 tgtvec, addutil::Vector3 jumpvec)
                    : PlayerControlMessage(plid, tgtvec)
                    , m_jumpvec(jumpvec)
                {
                }
                virtual ~HeadPlayerControlMessage() { }
                const std::string toString() const
                {
                    std::ostringstream oss(std::ostringstream::out);
                    oss << m_jumpvec.x << " " << m_jumpvec.y << " " << m_jumpvec.z;
                    return contString(c_pl_ctl_hold, true, oss.str());
                }

            private:
                addutil::Vector3 m_jumpvec;
            };
        }
    }
}

#endif
