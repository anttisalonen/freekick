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


#ifndef FREEKICK_MATCH_MESSAGES_LISTPARAMETERMESSAGE_H
#define FREEKICK_MATCH_MESSAGES_LISTPARAMETERMESSAGE_H

#include "ParameterMessage.h"

namespace freekick
{
    namespace match
    {
        namespace messages
        {
            class ListParameterMessage : public ParameterMessage
            {
            public:
                ListParameterMessage(std::string& msg, const std::string& corr_id)
                {
                    using namespace boost;
                    std::string exp(" *?\\(A +([[:print:]]+?)\\).*");
                    exp.replace(exp.find("A"), 1, corr_id);
                    regex expr(exp.c_str());
                    cmatch what;
                    if(regex_match(msg.c_str(), what, expr))
                    {
                        std::string s1;
                        s1.assign(what[1].first, what[1].second);
                        bool successful = messageListToSet(s1, m_values);
                        if(!successful) throw "ListParameterMessage: failed parse";
                    }
                    else
                        throw "ListParameterMessage: failed parse";
                }
                virtual ~ListParameterMessage() { }
                const std::set<PlayerID>& getList() const { return m_values; }

            protected:
                ListParameterMessage()
                {
                }
                ListParameterMessage(PlayerID pl)
                {
                    m_values.insert(pl);
                }
                ListParameterMessage(const std::set<PlayerID>& vals)
                    : m_values(vals)
                {
                }
                const std::string listParamString(const std::string& type) const
                {
                    return paramString(type, setToMessageList(m_values));
                }

            private:
                std::set<PlayerID> m_values;
            };
        }
    }
}

#endif
