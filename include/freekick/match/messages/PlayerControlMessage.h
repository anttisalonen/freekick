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


#ifndef FREEKICK_MATCH_MESSAGES_PLAYERCONTROLMESSAGE_H
#define FREEKICK_MATCH_MESSAGES_PLAYERCONTROLMESSAGE_H

#include "Vector3.h"

#include "ParameterMessage.h"

namespace freekick
{
    namespace match
    {
        namespace messages
        {
            class PlayerControlMessage : public ParameterMessage
            {
            public:
                PlayerControlMessage(PlayerID plid, addutil::Vector3 tgtvec)
                    : m_plid(plid)
                    , m_tgtvec(tgtvec)
                {
                }
                virtual ~PlayerControlMessage() { }

            protected:
                const std::string contString(const std::string& type, bool incl_tgtvec = true, const std::string& extra_info = "") const
                {
                    std::ostringstream oss(std::ostringstream::out);
                    oss << m_plid;
                    if(incl_tgtvec)
                        oss << " " << m_tgtvec.x << " " << m_tgtvec.y << " " << m_tgtvec.z;
                    if(!extra_info.empty()) 
                        oss << " " << extra_info;
                    return paramString(type, oss.str());
                }

            private:
                PlayerID m_plid;
                addutil::Vector3 m_tgtvec;
            };
        }
    }
}

#endif
