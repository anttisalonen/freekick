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


#ifndef FREEKICK_MATCH_MESSAGES_GENERALUPDATESTATUSMESSAGE_H
#define FREEKICK_MATCH_MESSAGES_GENERALUPDATESTATUSMESSAGE_H

#include "addutil/Vector3.h"

#include "BallState.h"

#include "GeneralUpdateMessage.h"

namespace freekick
{
    namespace match
    {
        namespace messages
        {
            class GeneralUpdateStatusMessage : public GeneralUpdateMessage
            {
            public:
                GeneralUpdateStatusMessage(BallState bs)
                    : m_bs(bs)
                {
                }

                GeneralUpdateStatusMessage(BallInOut bs, int owner, addutil::Vector3 resp, bool blocked)
                {
                    m_bs.bio_type = bs;
                    m_bs.owner = intToBallOwner(owner);
                    m_bs.restart_point = resp;
                    m_bs.blocked_play = blocked;
                }

                GeneralUpdateStatusMessage(std::string& msg)
                {
                    using namespace boost;
                    // TODO: read s_gen_status_upd from Message.h
                    regex expr(" *?\\(D +([0-9]{1,2}?) +([01]) +([[:print:]]+?) +([[:print:]]+?) +([01])\\).*");
                    cmatch what;
                    if(regex_match(msg.c_str(), what, expr))
                    {
                        std::string s1, s2, s3, s4, s5;
                        s1.assign(what[1].first, what[1].second);
                        s2.assign(what[2].first, what[2].second);
                        s3.assign(what[3].first, what[3].second);
                        s4.assign(what[4].first, what[4].second);
                        s5.assign(what[5].first, what[5].second);
                        int bss = atoi(s1.c_str());
                        m_bs.bio_type = intToBallInOut(bss);
                        m_bs.owner = intToBallOwner(atoi(s2.c_str()));
                        m_bs.restart_point.x = atof(s3.c_str());
                        m_bs.restart_point.y = 0.0f;
                        m_bs.restart_point.z = atof(s4.c_str());
                        m_bs.blocked_play = atoi(s5.c_str());
                    }
                    else
                        throw "GeneralUpdateStatusMessage: failed parse";
                }

                virtual ~GeneralUpdateStatusMessage() { }

                const std::string toString() const
                {
                    std::ostringstream oss(std::ostringstream::out);
                    oss << s_gen_status_upd << " " << m_bs.bio_type << " " << m_bs.owner << " " << m_bs.restart_point.x << " " << m_bs.restart_point.z << " " << m_bs.blocked_play;
                    return stdString(oss.str());
                }

                const BallState& getBallState() const { return m_bs; }

            private:
                BallState m_bs;
            };
        }
    }
}

#endif
