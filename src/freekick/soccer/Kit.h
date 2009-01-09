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
  This file was generated on So Okt 26 2008 at 12:09:20
**************************************************************************/


#ifndef KIT_H
#define KIT_H

#include <string>
#include <vector>

#include <boost/array.hpp>
#include <boost/serialization/vector.hpp>
#include <boost/serialization/string.hpp>

#include "addutil/Color.h"

/**
 * class Kit
 */

namespace freekick
{
    namespace soccer
    {
        using addutil::Color;
        class Kit
        {
        public:
            Kit (int jtype, const Color& jcolor1, const Color& jcolor2, const Color& shortscol, const Color& sockscol );
            int getJerseyType() const;
            void getFirstJerseyColor(Color& c) const;
            void getSecondJerseyColor(Color& c) const;
            void getShortsColor(Color& c) const;
            void getSocksColor(Color& c) const;

        private:
            int jerseytype;
            boost::array<Color, 2> jerseycolors;
            std::string jerseypic;
            Color shortscolor;
            Color sockscolor;

            friend class boost::serialization::access;
            template<class Archive>
                void serialize(Archive & ar, const unsigned int version)
            {
                ar & jerseytype;
                ar & jerseycolors;
                ar & jerseypic;
                ar & shortscolor;
                ar & sockscolor;
            }
        };
    }
}

#endif // KIT_H
