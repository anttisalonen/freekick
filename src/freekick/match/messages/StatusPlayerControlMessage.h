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


#ifndef FREEKICK_MATCH_MESSAGES_STATUSPLAYERCONTROLMESSAGE_H
#define FREEKICK_MATCH_MESSAGES_STATUSPLAYERCONTROLMESSAGE_H

#include "PlayerControlMessage.h"

namespace freekick
{
    namespace match
    {
        namespace messages
        {
            class StatusPlayerControlMessage : public PlayerControlMessage
            {
            public:
                StatusPlayerControlMessage(PlayerID plid, const ControlledStatus& cs)
                    : PlayerControlMessage(plid)
                    , m_cs(cs)
                {
                }
                virtual ~StatusPlayerControlMessage() { }
                const std::string toString() const
                {
                    std::ostringstream oss(std::ostringstream::out);
                    oss << m_cs;
                    return contString(c_pl_ctl_hold, false, oss.str());
                }

            private:
                ControlledStatus m_cs;
            };
        }
    }
}

#endif
