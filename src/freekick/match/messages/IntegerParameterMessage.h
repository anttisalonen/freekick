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
                IntegerParameterMessage()
                {
                }
                virtual ~IntegerParameterMessage() { }

            protected:
                const std::string intParamString(const std::string& type, int param) const
                {
                    std::ostringstream oss(std::ostringstream::out);
                    oss << param;
                    return paramString(type, oss.str());
                }

            private:
            };
        }
    }
}

#endif
