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
  This file was generated on So Okt 26 2008 at 12:09:20
**************************************************************************/

#include "Kit.h"

namespace freekick
{
    namespace soccer
    {
        Kit::Kit (int jtype, const Color& jcolor1, const Color& jcolor2, const Color& shortscol, const Color& sockscol ) 
            : jerseytype(jtype), jerseypic(""), shortscolor(shortscol), sockscolor(sockscol)
        {
            jerseycolors[0] = jcolor1;
            jerseycolors[1] = jcolor2;
        }

        Kit::Kit(xmlNodePtr root)
        {
            // TODO
        }

        int Kit::getJerseyType() const
        {
            return jerseytype;
        }

        void Kit::getFirstJerseyColor(Color& c) const
        {
            c = jerseycolors[0];
        }

        void Kit::getSecondJerseyColor(Color& c) const
        {
            c = jerseycolors[1];
        }

        void Kit::getShortsColor(Color& c) const
        {
            c = shortscolor;
        }

        void Kit::getSocksColor(Color& c) const
        {
            c = sockscolor;
        }
    }
}
