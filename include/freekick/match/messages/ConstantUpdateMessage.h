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


#ifndef FREEKICK_MATCH_MESSAGES_CONSTANTUPDATEMESSAGE_H
#define FREEKICK_MATCH_MESSAGES_CONSTANTUPDATEMESSAGE_H

#include "Vector3.h"
#include "Quaternion.h"

#include "CausedStatus.h"
#include "ControlledStatus.h"

#include "StandardMessage.h"

namespace freekick
{
    namespace match
    {
        namespace messages
        {
            class ConstantUpdateMessage : public StandardMessage
            {
            public:
                ConstantUpdateMessage(PlayerID plid, int der, 
                                      addutil::Vector3 vec, addutil::Quaternion quat, 
                                      CausedStatus causs = CausedStatus(), ControlledStatus contrs = ControlledStatus())
                    : m_plid(plid)
                    , m_der(der)
                    , m_vec(vec)
                    , m_quat(quat)
                    , m_causs(causs)
                    , m_contrs(contrs)
                {
                }
                virtual ~ConstantUpdateMessage() { }
                const std::string toString() const
                {
                    std::ostringstream oss(std::ostringstream::out);
                    oss << m_plid << " " << m_der << " " 
                        << m_vec.x << " " << m_vec.y << " " << m_vec.z << " " 
                        << m_quat.w << " " << m_quat.x << " " << m_quat.y << " " << m_quat.z << " "
                        << m_causs << " " << m_contrs;
                    return stdString(oss.str());
                }

            private:
                PlayerID m_plid;
                int m_der;
                addutil::Vector3 m_vec;
                addutil::Quaternion m_quat;
                CausedStatus m_causs;
                ControlledStatus m_contrs;
            };
        }
    }
}

#endif
