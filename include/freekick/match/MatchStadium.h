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


#ifndef MATCHSTADIUM_H
#define MATCHSTADIUM_H

#include <string>

#include <boost/serialization/base_object.hpp>

#include "StaticEntity.h"
#include "Stadium.h"
#include "MatchIDs.h"

/**
 * class Stadium
 */

namespace freekick
{
    namespace match
    {
        class MatchStadium : public freekick::soccer::Stadium, public addutil::StaticEntity
        {
        public:

            MatchStadium (const Stadium& s);
            const int getID() const { return StadiumID; }

        private:

            friend class boost::serialization::access;
            template<class Archive>
                void serialize(Archive & ar, const unsigned int version)
            {
                ar & boost::serialization::base_object<freekick::soccer::Stadium>(*this);
                ar & boost::serialization::base_object<addutil::StaticEntity>(*this);
                ar & StadiumID;
            }
        };
    }
}

#endif // MATCHSTADIUM_H
