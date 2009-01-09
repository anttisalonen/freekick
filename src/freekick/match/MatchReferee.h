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


#ifndef MATCHREFEREE_H
#define MATCHREFEREE_H

#include <string>

#include <boost/serialization/base_object.hpp>

#include "addutil/DynamicEntity.h"

#include "Kit.h"
#include "Referee.h"
#include "MatchIDs.h"

/**
 * class Referee
 */

namespace freekick
{
    namespace match
    {
        class MatchReferee : public freekick::soccer::Referee
        {
        public:

            /**
             */
            MatchReferee (const freekick::soccer::Referee& r);
            const int getID() const { return RefereeID; }

        private:

            freekick::soccer::Kit* kit;

            friend class boost::serialization::access;
            template<class Archive>
                void serialize(Archive & ar, const unsigned int version)
            {
                ar & boost::serialization::base_object<freekick::soccer::Referee>(*this);
                ar & kit;
            }
        };
    }
}

#endif // MATCHREFEREE_H
