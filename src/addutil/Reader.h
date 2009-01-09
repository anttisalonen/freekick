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


#ifndef ADDUTIL_READER_H
#define ADDUTIL_READER_H

/**
 * class Reader
 */

namespace addutil
{
    template <class T>
    class Reader
    {
    public:
        virtual ~Reader() { }
        virtual void update(T* publisher) = 0;
    };
}

#endif // ADDUTIL_READER_H
