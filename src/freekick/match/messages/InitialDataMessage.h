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


#ifndef FREEKICK_MATCH_MESSAGES_INITIALDATAMESSAGE_H
#define FREEKICK_MATCH_MESSAGES_INITIALDATAMESSAGE_H

#include <boost/archive/text_oarchive.hpp>

#include "MatchStatus.h"

#include "SerializationDataMessage.h"

namespace freekick
{
    namespace match
    {
        namespace messages
        {
            class InitialDataMessage : public SerializationDataMessage
            {
            public:
                InitialDataMessage(const MatchStatus& ms)
                    : SerializationDataMessage(initialdata_id)
                    , m_ms(ms)
                {
                }
                virtual ~InitialDataMessage() { }

                const std::string toString () const
                {
                    std::ostringstream oss(std::ostringstream::out);
                    boost::archive::text_oarchive oa(oss);
                    oa << m_ms;
                    return serString(oss.str());
                }

            private:
                const MatchStatus& m_ms;
            };
        }
    }
}

#endif
