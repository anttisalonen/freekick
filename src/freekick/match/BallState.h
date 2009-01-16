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


#ifndef BALLSTATE_H
#define BALLSTATE_H

#include <boost/serialization/serialization.hpp>

#include "addutil/Vector3.h"

#include "Primitives.h"

namespace freekick
{
    namespace match
    {
        enum BallInOut
        {
            BallIn = 0,
            PreKickoff,
            Kickoff,
            Throwin,
            Goalkick,
            Cornerkick,
            IndirectFreekick,
            DirectFreekick,
            PenaltyKick,
            DroppedBall,
            HalfFullTime
        };

        class BallState
        {
            // TODO: more controlled access
        public:
            BallState();
            void flipOwner();

            BallInOut bio_type;
            soccer::BallOwner owner;
            addutil::Vector3 restart_point;
            bool blocked_play;


            friend class boost::serialization::access;
            template<class Archive>
                void serialize(Archive & ar, const unsigned int version)
            {
                ar & bio_type;
                ar & owner;
                ar & restart_point;
                ar & blocked_play;
            }
        };

        BallInOut intToBallInOut(int n);
        std::ostream& operator<<(std::ostream& os, const BallState& e);
    }
}

#endif
