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


#ifndef FREEKICK_MATCH_MESSAGES_USERNAMEREQUESTMESSAGE_H
#define FREEKICK_MATCH_MESSAGES_USERNAMEREQUESTMESSAGE_H

#include "StringParameterMessage.h"

namespace freekick
{
    namespace match
    {
        namespace messages
        {
            class UsernameRequestMessage : public StringParameterMessage
            {
            public:
                UsernameRequestMessage(const std::string& username)
                    : m_username(username)
                {
                }
                virtual ~UsernameRequestMessage() { }
                const std::string toString() const
                {
                    return strParamString(c_username_req, m_username);
                }

            private:
                std::string m_username;
            };
        }
    }
}

#endif
