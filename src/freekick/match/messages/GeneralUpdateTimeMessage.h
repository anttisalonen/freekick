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


#ifndef FREEKICK_MATCH_MESSAGES_GENERALUPDATETIMEMESSAGE_H
#define FREEKICK_MATCH_MESSAGES_GENERALUPDATETIMEMESSAGE_H

#include "GeneralUpdateMessage.h"

namespace freekick
{
    namespace match
    {
        namespace messages
        {
            class GeneralUpdateTimeMessage : public GeneralUpdateMessage
            {
            public:
                GeneralUpdateTimeMessage(unsigned int min, unsigned int sec, unsigned int half)
                    : m_min(min)
                    , m_sec(sec)
                    , m_half(half)
                {
                }
                virtual ~GeneralUpdateTimeMessage() { }
                const std::string toString() const
                {
                    std::ostringstream oss(std::ostringstream::out);
                    oss << s_gen_time_upd << " " << m_min << " " << m_sec << " " << m_half;
                    return stdString(oss.str());
                }

            private:
                unsigned int m_min, m_sec, m_half;
            };
        }
    }
}

#endif
