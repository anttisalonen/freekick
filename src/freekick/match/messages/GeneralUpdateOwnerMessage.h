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


#ifndef FREEKICK_MATCH_MESSAGES_GENERALUPDATEOWNERMESSAGE_H
#define FREEKICK_MATCH_MESSAGES_GENERALUPDATEOWNERMESSAGE_H

#include "addutil/Vector3.h"

#include "BallState.h"

#include "GeneralUpdateMessage.h"

namespace freekick
{
    namespace match
    {
        namespace messages
        {
            class GeneralUpdateOwnerMessage : public GeneralUpdateMessage
            {
            public:
                GeneralUpdateOwnerMessage(int plid)
                    : m_id(plid)
                {
                }

                GeneralUpdateOwnerMessage(std::string& msg)
                {
                    using namespace boost;
                    // TODO: read s_gen_owner_upd from Message.h
                    regex expr(" *?\\(E +([-0-9]+).*");
                    cmatch what;
                    if(regex_match(msg.c_str(), what, expr))
                    {
                        std::string s1;
                        s1.assign(what[1].first, what[1].second);
                        m_id = atoi(s1.c_str());
                    }
                    else
                        throw "GeneralUpdateOwnerMessage: failed parse";
                }

                virtual ~GeneralUpdateOwnerMessage() { }

                const std::string toString() const
                {
                    std::ostringstream oss(std::ostringstream::out);
                    oss << s_gen_owner_upd << " " << m_id;
                    return stdString(oss.str());
                }

                int getOwnerID() const { return m_id; }

            private:
                int m_id;
            };
        }
    }
}

#endif
