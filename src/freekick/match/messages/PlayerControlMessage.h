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


#ifndef FREEKICK_MATCH_MESSAGES_PLAYERCONTROLMESSAGE_H
#define FREEKICK_MATCH_MESSAGES_PLAYERCONTROLMESSAGE_H

#include <boost/regex.hpp>

#include "addutil/Vector3.h"

#include "ParameterMessage.h"

namespace freekick
{
    namespace match
    {
        namespace messages
        {
            class PlayerControlMessage : public ParameterMessage
            {
            public:
                PlayerControlMessage(PlayerID plid, addutil::Vector3 tgtvec = addutil::Vector3())
                    : m_plid(plid)
                    , m_tgtvec(tgtvec)
                {
                }
                virtual ~PlayerControlMessage() { }

                PlayerID getPlayerID() const { return m_plid; }
                void getTargetVector(addutil::Vector3& t) const { t = m_tgtvec; }

            protected:
                PlayerControlMessage(std::string& msg, const std::string& corr_id)
                {
                    using namespace boost;
                    // TODO: read action types from Message.h
                    regex expr(".*?\\((a|b|c|d|e|f) +([-0-9]+) +([[:print:]]+?) +([[:print:]]+?) +([[:print:]]+?)( |\\))(.*)");
                    cmatch what;
                    if(regex_match(msg.c_str(), what, expr))
                    {
                        std::string s1, s2, s3, s4, this_id;
                        this_id.assign(what[1].first, what[1].second);
                        if (this_id != corr_id) throw "PlayerControlMessage: incorrect ID\n";
                        s1.assign(what[2].first, what[2].second);
                        s2.assign(what[3].first, what[3].second);
                        s3.assign(what[4].first, what[4].second);
                        s4.assign(what[5].first, what[5].second);
                        m_plid = atoi(s1.c_str());
                        m_tgtvec.x = atof(s2.c_str());
                        m_tgtvec.y = atof(s3.c_str());
                        m_tgtvec.z = atof(s4.c_str());
                        msg.assign(what[7].first, what[7].second);
                    }
                    else
                        throw "PlayerControlMessage: failed parse";                    
                }

                const std::string contString(const std::string& type, bool incl_tgtvec = true, const std::string& extra_info = "") const
                {
                    std::ostringstream oss(std::ostringstream::out);
                    oss << m_plid;
                    if(incl_tgtvec)
                        oss << " " << m_tgtvec.x << " " << m_tgtvec.y << " " << m_tgtvec.z;
                    if(!extra_info.empty()) 
                        oss << " " << extra_info;
                    return paramString(type, oss.str());
                }

            private:
                PlayerID m_plid;
                addutil::Vector3 m_tgtvec;
            };
        }
    }
}

#endif
