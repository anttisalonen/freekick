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


#ifndef FREEKICK_MATCH_MESSAGES_SETCONSTANTUPDATEINTERVALMESSAGE_H
#define FREEKICK_MATCH_MESSAGES_SETCONSTANTUPDATEINTERVALMESSAGE_H

#include "IntegerParameterMessage.h"

namespace freekick
{
    namespace match
    {
        namespace messages
        {
            class SetConstantUpdateIntervalMessage : public IntegerParameterMessage
            {
            public:
                SetConstantUpdateIntervalMessage(int interval)
                    : IntegerParameterMessage(interval)
                {
                }
                SetConstantUpdateIntervalMessage(std::string& msg)
                    : IntegerParameterMessage(msg, c_set_const_upd_int)
                {
                }

                virtual ~SetConstantUpdateIntervalMessage() { }
                const std::string toString() const
                {
                    return intParamString(c_set_const_upd_int);
                }
            };
        }
    }
}

#endif
