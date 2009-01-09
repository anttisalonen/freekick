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


#ifndef ADDUTIL_PUBLISHER_H
#define ADDUTIL_PUBLISHER_H

#include <set>

#include <boost/foreach.hpp>

#include "Reader.h"

/**
 * class Publisher
 */

namespace addutil
{
    template <class T>
        class Publisher
    {
    public:
        Publisher() { }
        virtual ~Publisher() { }
        void publish()
        {
            BOOST_FOREACH(Reader<T>* r, mReaders)
            {
                r->update(static_cast<T *>(this));
            }
        }

        void subscribe(Reader<T>& reader)
        {
            mReaders.insert(&reader);
        }

        void unsubscribe(Reader<T>& reader)
        {
            mReaders.erase(&reader);
        }
    private:
        std::set<Reader<T>*> mReaders;
    };
}

#endif // ADDUTIL_PUBLISHER_H
