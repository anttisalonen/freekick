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


#ifndef FREEKICK_MATCH_MESSAGES_SETGENERALUPDATEINTERVALMESSAGE_H
#define FREEKICK_MATCH_MESSAGES_SETGENERALUPDATEINTERVALMESSAGE_H

#include "IntegerParameterMessage.h"

namespace freekick
{
    namespace match
    {
        namespace messages
        {
            class SetGeneralUpdateIntervalMessage : public IntegerParameterMessage
            {
            public:
                SetGeneralUpdateIntervalMessage(int interval)
                    : m_interval(interval)
                {
                }
                virtual ~SetGeneralUpdateIntervalMessage() { }
                const std::string toString() const
                {
                    return intParamString(c_set_gen_upd_int, m_interval);
                }

            private:
                int m_interval;
            };
        }
    }
}

#endif
