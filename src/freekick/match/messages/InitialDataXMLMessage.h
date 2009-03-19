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


#ifndef FREEKICK_MATCH_MESSAGES_INITIALDATAXMLMESSAGE_H
#define FREEKICK_MATCH_MESSAGES_INITIALDATAXMLMESSAGE_H

#include <sstream>

#include <boost/archive/text_oarchive.hpp>

#include "addutil/XML.h"

#include "MatchStatus.h"

#include "SerializationDataMessage.h"

namespace freekick
{
    namespace match
    {
        namespace messages
        {
            class InitialDataXMLMessage : public SerializationDataMessage
            {
            public:
                InitialDataXMLMessage(const xmlDocPtr doc)
                    : SerializationDataMessage(initialdata_xml_id)
                {
                    addutil::xml::node_to_stringstream(xmlDocGetRootElement(doc), m_ser_stream, false);
                }
                InitialDataXMLMessage(std::string& msg)
                    : SerializationDataMessage(msg, initialdata_xml_id)
                {
                    boost::shared_ptr<MatchData> data(new MatchData(msg));
                    m_status.reset(new MatchStatus(data));
                }

                virtual ~InitialDataXMLMessage() { }

                const std::string toString () const
                {
                    return serString(m_ser_stream.str());
                }

                boost::shared_ptr<MatchStatus> getMatchStatus() const
                {
                    return m_status;
                }

            private:
                std::ostringstream m_ser_stream;
                boost::shared_ptr<MatchStatus> m_status;
            };
        }
    }
}

#endif
