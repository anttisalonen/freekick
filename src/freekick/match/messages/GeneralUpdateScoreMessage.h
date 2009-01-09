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
                virtual ~GeneralUpdateScoreMessage() { }
                const std::string toString() const
                {
                    std::ostringstream oss(std::ostringstream::out);
                    oss << s_gen_score_upd << " " << m_hg << " " << m_ag << " " << m_hp << " " << m_ap;
                    return stdString(oss.str());
                }

            private:
                unsigned int m_hg, m_ag, m_hp, m_ap;
            };
        }
    }
}

#endif
