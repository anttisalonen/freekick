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
                ListParameterMessage()
                {
                }
                virtual ~ListParameterMessage() { }

            protected:
                const std::string listParamString(const std::string& type, const std::set<PlayerID>& s) const
                {
                    return paramString(type, setToMessageList(s));
                }

            private:
            };
        }
    }
}

#endif
