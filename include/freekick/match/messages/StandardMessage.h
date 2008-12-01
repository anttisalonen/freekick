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


#ifndef FREEKICK_MATCH_MESSAGES_STANDARDMESSAGE_H
#define FREEKICK_MATCH_MESSAGES_STANDARDMESSAGE_H

#include "Message.h"

namespace freekick
{
    namespace match
    {
        namespace messages
        {
            class StandardMessage : public Message
            {
            public:
                virtual ~StandardMessage() { }

            protected:
                const std::string stdString(const std::string& msg) const
                {
                    return (msg_start + msg + msg_end);
                }

            private:
            };
        }
    }
}

#endif
