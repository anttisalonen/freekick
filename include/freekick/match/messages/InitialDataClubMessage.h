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


#ifndef FREEKICK_MATCH_MESSAGES_INITIALDATACLUBMESSAGE_H
#define FREEKICK_MATCH_MESSAGES_INITIALDATACLUBMESSAGE_H

#include <string>

#include "SerializationDataMessage.h"

namespace freekick
{
    namespace match
    {
        namespace messages
        {
            class InitialDataClubMessage : public SerializationDataMessage
            {
            public:
                InitialDataClubMessage(const std::string& c1, const std::string& c2)
                    : SerializationDataMessage(initialdata_club_id),
                      club1name(c1),
                      club2name(c2)
                {
                }
                virtual ~InitialDataClubMessage() { }

                const std::string toString () const
                {
                    std::ostringstream oss(std::ostringstream::out);
                    oss << "\"" << club1name << "\" \"" << club2name << "\"";
                    return serString(oss.str());
                }

            private:
                std::string club1name;
                std::string club2name;
            };
        }
    }
}

#endif
