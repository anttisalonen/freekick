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


#ifndef FREEKICK_MATCH_MESSAGES_INITMESSAGE_H
#define FREEKICK_MATCH_MESSAGES_INITMESSAGE_H

#include "Message.h"

namespace freekick
{
    namespace match
    {
        namespace messages
        {
            class InitMessage : public Message
            {
            public:
                InitMessage(const std::string& identification, const std::string& name, const std::string& protocol)
                    : m_identification(identification)
                    , m_name(name)
                    , m_protocol(protocol)
                {
                }
                virtual ~InitMessage() { }

            protected:
                const std::string helperString() const
                {
                    std::ostringstream oss(std::ostringstream::out);
                    oss << m_identification << " \"" << m_name << "\" \""
                        << m_protocol << "\"";
                    return oss.str();
                }

            private:
                std::string m_identification;
                std::string m_name;
                std::string m_protocol;
            };
        }
    }
}

#endif
