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


#ifndef FREEKICK_MATCH_MESSAGES_SERVERINITMESSAGE_H
#define FREEKICK_MATCH_MESSAGES_SERVERINITMESSAGE_H

#include "InitMessage.h"

namespace freekick
{
    namespace match
    {
        namespace messages
        {
            class ServerInitMessage : public InitMessage
            {
            public:
                ServerInitMessage(const std::string& name, const std::string& protocol, const std::string& greeting)
                    : InitMessage(freekick_server_id, name, protocol)
                    , m_greeting(greeting)
                {
                }
                virtual ~ServerInitMessage() { }
                virtual const std::string toString() const
                {
                    std::ostringstream oss(std::ostringstream::out);
                    oss << helperString() << "\n" << m_greeting << "\n\n";
                    return oss.str();
                }

            private:
                std::string m_greeting;
            };
        }
    }
}

#endif
