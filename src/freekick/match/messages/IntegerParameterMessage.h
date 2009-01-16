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


#ifndef FREEKICK_MATCH_MESSAGES_INTEGERPARAMETERMESSAGE_H
#define FREEKICK_MATCH_MESSAGES_INTEGERPARAMETERMESSAGE_H

#include "ParameterMessage.h"

namespace freekick
{
    namespace match
    {
        namespace messages
        {
            class IntegerParameterMessage : public ParameterMessage
            {
            public:
                IntegerParameterMessage(std::string& msg, const std::string& corr_id)
                {
                    using namespace boost;
                    // TODO: read action types from Message.h
                    std::string exp(" *?\\(A +([0-9]+)\\).*");
                    exp.replace(exp.find("A"), 1, corr_id);
                    regex expr(exp.c_str());
                    cmatch what;
                    if(regex_match(msg.c_str(), what, expr))
                    {
                        std::string s1;
                        s1.assign(what[1].first, what[1].second);
                        m_value = atoi(s1.c_str());
                    }
                    else
                        throw "IntegerParameterMessage: failed parse";
                }
                virtual ~IntegerParameterMessage() { }
                int getValue() const { return m_value; }

            protected:
                IntegerParameterMessage(int v)
                    : m_value(v) { }
                const std::string intParamString(const std::string& type) const
                {
                    std::ostringstream oss(std::ostringstream::out);
                    oss << m_value;
                    return paramString(type, oss.str());
                }

            private:
                int m_value;
            };
        }
    }
}

#endif
