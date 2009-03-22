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

                GeneralUpdateTimeMessage(std::string& msg)
                {
                    using namespace boost;
                    // TODO: read s_gen_time_upd from Message.h
                    regex expr(" *?\\(B +([0-9]{1,2}?) +([0-9]{1,2}?) +([0-4])\\).*");
                    cmatch what;
                    if(regex_match(msg.c_str(), what, expr))
                    {
                        std::string s1, s2, s3;
                        s1.assign(what[1].first, what[1].second);
                        s2.assign(what[2].first, what[2].second);
                        s3.assign(what[3].first, what[3].second);
                        m_min  = atoi(s1.c_str());
                        m_sec  = atoi(s2.c_str());
                        m_half = atoi(s3.c_str());
                    }
                    else
                        throw "GeneralUpdateTimeMessage: failed parse";
                }

                virtual ~GeneralUpdateTimeMessage() { }
                const std::string toString() const
                {
                    std::ostringstream oss(std::ostringstream::out);
                    oss << s_gen_time_upd << " " << m_min << " " << m_sec << " " << m_half;
                    return stdString(oss.str());
                }

                unsigned int m_min, m_sec, m_half;
            };
        }
    }
}

#endif
