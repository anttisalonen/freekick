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


#ifndef FREEKICK_MATCH_MESSAGES_NEWUSERNAMEACK_H
#define FREEKICK_MATCH_MESSAGES_NEWUSERNAMEACK_H

#include "SingularMessage.h"

namespace freekick
{
    namespace match
    {
        namespace messages
        {
            class NewUsernameAck : public SingularMessage
            {
            public:
                virtual ~NewUsernameAck() { }
                const std::string toString() const
                {
                    return singString(s_username_ack);
                }
            };
        }
    }
}

#endif
