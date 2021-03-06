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


#ifndef FREEKICK_MATCH_MESSAGES_CLIENTINITMESSAGE_H
#define FREEKICK_MATCH_MESSAGES_CLIENTINITMESSAGE_H

#include "InitMessage.h"

namespace freekick
{
    namespace match
    {
        namespace messages
        {
            class ClientInitMessage : public InitMessage
            {
            public:
                ClientInitMessage(const std::string& name, 
                                  const std::string& protocol, 
                                  const std::string& username = "", 
                                  const std::string& type = "H")
                    : InitMessage(freekick_client_id, name, protocol)
                    , m_username(username)
                    , m_type(type)
                {
                }
                ClientInitMessage(std::string& msg)
                    : InitMessage(msg)
                {
                    using namespace boost;
                    regex expr(" *\"([[:print:]]+?)\" +([[:print:]]).*");
                    cmatch what;
                    if(regex_match(msg.c_str(), what, expr))
                    {
                        m_username.assign(what[1].first, what[1].second);
                        m_type.assign(what[2].first, what[2].second);
                    }
                    else
                        throw "ClientInitMessage: failed parse";
                }

                virtual ~ClientInitMessage() { }
                const std::string toString() const
                {
                    std::ostringstream oss(std::ostringstream::out);
                    oss << helperString() << " \"" << m_username << "\" " << m_type << "\n";
                    return oss.str();
                }
                void getUsername(std::string& n) const
                {
                    n = m_username;
                }
                void getType(std::string& n) const
                {
                    n = m_type;
                }

            private:
                std::string m_username;
                std::string m_type;
            };
        }
    }
}

#endif
