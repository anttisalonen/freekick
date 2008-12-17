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


#ifndef FREEKICK_MATCH_MESSAGES_INITIALDATAKITMESSAGE_H
#define FREEKICK_MATCH_MESSAGES_INITIALDATAKITMESSAGE_H

#include <string>
#include <vector>

#include <boost/foreach.hpp>

#include "Color.h"

#include "Kit.h"

#include "SerializationDataMessage.h"

namespace freekick
{
    namespace match
    {
        namespace messages
        {
            class InitialDataKitMessage : public SerializationDataMessage
            {
            public:
                InitialDataKitMessage(const Kit& homekit, const Kit& awaykit, const Kit& hgkkit, const Kit& agkkit, const Kit& refkit)
                    : SerializationDataMessage(initialdata_kit_id)
                {
                    mKits.push_back(homekit);
                    mKits.push_back(awaykit);
                    mKits.push_back(hgkkit);
                    mKits.push_back(agkkit);
                    mKits.push_back(refkit);
                }
                virtual ~InitialDataKitMessage() { }

                const std::string toString () const
                {
                    std::ostringstream oss(std::ostringstream::out);
                    BOOST_FOREACH(Kit k, mKits)
                    {
                        Color c;
                        oss << k.getJerseyType() << " ";
                        k.getFirstJerseyColor(c);
                        oss << c << " ";
                        k.getSecondJerseyColor(c);
                        oss << c << " ";
                        k.getShortsColor(c);
                        oss << c << " ";
                        k.getSocksColor(c);
                        oss << c << " ";
                    }
                    return serString(oss.str());
                }

            private:
                std::vector<Kit> mKits;
            };
        }
    }
}

#endif
