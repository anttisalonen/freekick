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


#ifndef FREEKICK_MATCH_MESSAGES_SERIALIZATIONDATAMESSAGE_H
#define FREEKICK_MATCH_MESSAGES_SERIALIZATIONDATAMESSAGE_H

#include "StandardMessage.h"

namespace freekick
{
    namespace match
    {
        namespace messages
        {
            class SerializationDataMessage : public StandardMessage
            {
            public:
                SerializationDataMessage(unsigned int id)
                    : m_serializationid(id)
                {
                }
                virtual ~SerializationDataMessage() { }

            protected:
                SerializationDataMessage(std::string& msg, int corr_id)
                {
                    using namespace boost;
                    regex expr(".*?\\(\\+ ([0-9]{3}) ([[:print:]]+?) \\+\\).*");
                    cmatch what;
                    if(regex_match(msg.c_str(), what, expr))
                    {
                        std::string s1;
                        s1.assign(what[1].first, what[1].second);
                        msg.assign(what[2].first, what[2].second);
                        int this_id = atoi(s1.c_str());
                        if(corr_id != this_id) throw addutil::Exception("SerializationDataMessage: incorrect ID");
                    }
                    else
                        throw addutil::Exception("SerializationDataMessage: failed parse");
                }

                const std::string serString(const std::string& msg) const
                {
                    std::ostringstream oss(std::ostringstream::out);
                    oss << serialization_delim << " " << m_serializationid << " " << msg << " " << serialization_delim;
                    return stdString(oss.str());
                }

            private:
                unsigned int m_serializationid;
            };
        }
    }
}

#endif
