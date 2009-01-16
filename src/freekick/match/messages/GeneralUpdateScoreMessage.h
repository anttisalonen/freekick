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


#ifndef FREEKICK_MATCH_MESSAGES_GENERALUPDATESCOREMESSAGE_H
#define FREEKICK_MATCH_MESSAGES_GENERALUPDATESCOREMESSAGE_H

#include "GeneralUpdateMessage.h"

namespace freekick
{
    namespace match
    {
        namespace messages
        {
            class GeneralUpdateScoreMessage : public GeneralUpdateMessage
            {
            public:
                GeneralUpdateScoreMessage(unsigned int hg, unsigned int ag, unsigned int hp = 0, unsigned int ap = 0)
                    : m_hg(hg)
                    , m_ag(ag)
                    , m_hp(hp)
                    , m_ap(ap)
                {
                }
                GeneralUpdateScoreMessage(std::string& msg)
                {
                    using namespace boost;
                    // TODO: read s_gen_score_upd from Message.h
                    regex expr(" *?\\(C +([0-9]{1,2}?) +([0-9]{1,2}?) +([0-9]{1,2}?) +([0-9]{1,2}?) *\\).*");
                    cmatch what;
                    if(regex_match(msg.c_str(), what, expr))
                    {
                        std::string s1, s2, s3, s4;
                        s1.assign(what[1].first, what[1].second);
                        s2.assign(what[2].first, what[2].second);
                        s3.assign(what[3].first, what[3].second);
                        s4.assign(what[4].first, what[4].second);
                        m_hg = atoi(s1.c_str());
                        m_ag = atoi(s2.c_str());
                        m_hp = atoi(s3.c_str());
                        m_ap = atoi(s4.c_str());
                    }
                    else
                        throw "GeneralUpdateScoreMessage: failed parse";
                }

                virtual ~GeneralUpdateScoreMessage() { }
                const std::string toString() const
                {
                    std::ostringstream oss(std::ostringstream::out);
                    oss << s_gen_score_upd << " " << m_hg << " " << m_ag << " " << m_hp << " " << m_ap;
                    return stdString(oss.str());
                }

                int goals(bool home, bool penalty) const
                {
                    if(home)
                    {
                        if(penalty)
                            return m_hp;
                        else
                            return m_hg;
                    }
                    else
                    {
                        if(penalty)
                            return m_ap;
                        else
                            return m_ag;
                    }
                }
            private:
                int m_hg, m_ag, m_hp, m_ap;
            };
        }
    }
}

#endif
