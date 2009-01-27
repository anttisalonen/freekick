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


#ifndef MATCHBALL_H
#define MATCHBALL_H

#include <boost/shared_ptr.hpp>
#include <boost/serialization/base_object.hpp>

#include "addutil/DynamicEntity.h"

#include "Ball.h"

#include "MatchIDs.h"

/**
 * class Ball
 */

namespace freekick
{
    namespace match
    {
        class MatchBall : public addutil::DynamicEntity, public freekick::soccer::Ball
        {
        public:

            /**
             * @param  _mass
             */
            MatchBall (const Ball& b);
            const int getID() const { return BallID; }
            int getHolder() const;
            void setHolder(int h);

        private:
            int holder;

            friend class boost::serialization::access;
            template<class Archive>
                void serialize(Archive & ar, const unsigned int version)
            {
                ar & boost::serialization::base_object<addutil::DynamicEntity>(*this);
                ar & boost::serialization::base_object<freekick::soccer::Ball>(*this);
            }
        };
    }
}

#endif // MATCHBALL_H
