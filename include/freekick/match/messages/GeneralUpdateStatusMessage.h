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

#include "Vector3.h"

#include "BallStatus.h"

#include "GeneralUpdateStatusMessage.h"

namespace freekick
{
    namespace match
    {
        namespace messages
        {
            class GeneralUpdateStatusMessage : public GeneralUpdateMessage
            {
            public:
                GeneralUpdateStatusMessage(BallStatus bs, unsigned int owner, Vector3 resp, bool ref_cont)
                    : m_bs(bs)
                    , m_owner(owner)
                    , m_resp(resp)
                    , m_ref_cont(ref_cont)
                {
                }
                virtual ~GeneralUpdateStatusMessage() { }
                const std::string toString() const
                {
                    std::ostringstream oss(std::ostringstream::out);
                    oss << s_gen_pause_upd << " " << m_pause;
                    oss << s_gen_status_upd << " " << m_bs << " " << m_owner << " " << m_resp.x << " " << m_resp.z << " " << m_ref_conf;
                    return stdString(oss.str());
                }

            private:
                BallStatus m_bs;
                unsigned int m_owner;
                Vector3 m_resp;
                bool m_ref_cont;
            };
        }
    }
}

#endif
