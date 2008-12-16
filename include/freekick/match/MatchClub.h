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


#ifndef MATCHCLUB_H
#define MATCHCLUB_H

#include <string>
#include <vector>
#include <map>

#include <boost/shared_ptr.hpp>
#include <boost/foreach.hpp>
#include <boost/serialization/base_object.hpp>
#include <boost/serialization/map.hpp>
#include <boost/serialization/shared_ptr.hpp>

#include "Club.h"
#include "Kit.h"
#include "MatchPlayer.h"

/**
 * class MatchClub
 */

namespace freekick
{
    namespace match
    {
        using namespace freekick::soccer;
        class MatchClub : public Club
        {
        public:
            /**
             * @param  name
             */
            MatchClub (const Club& c);
            void addMatchPlayer(boost::shared_ptr<MatchPlayer> p);
            bool updatePlayer(int i, int v, float x, float y, float z, float qw = 0.0f, float qx = 0.0f, float qy = 0.0f, float qz = 0.0f);

        private:
            /* TODO: MatchClub has a map of MatchPlayers as well as a map of Players.
               The map of Players should be removed (polymorphism?). */
            std::map <int, boost::shared_ptr<MatchPlayer> > matchplayers;
            Kit* currkit;

            friend class boost::serialization::access;
            template<class Archive>
                void serialize(Archive & ar, const unsigned int version)
            {
                ar & boost::serialization::base_object<Club>(*this);
                ar & matchplayers;
                ar & currkit;
            }
        };
    }
}

#endif // MATCHCLUB_H
