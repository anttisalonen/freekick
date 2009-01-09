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


#ifndef PLAYER_H
#define PLAYER_H

#include <string>

#include <boost/serialization/base_object.hpp>

#include "addutil/Human.h"
#include "addutil/Color.h"

/**
 * class Player
 */

namespace freekick
{
    namespace soccer
    {
        enum PlayerPosition
        {
            Goalkeeper,
            Defender,
            Midfielder,
            Forward
        };

        class Player : public addutil::Human
        {
        public:

            // Constructors/Destructors
            //  

            /**
             * @param  name
             * @param  num
             */
            Player (const std::string& _name, int num, unsigned int _idnumber, PlayerPosition pos);
            unsigned int getID() const;
            int getNumber() const;
            PlayerPosition getPlayerPosition() const;

        private:

            int number;
            const unsigned int idnumber;
            PlayerPosition position;

            friend class boost::serialization::access;
            template<class Archive>
                void serialize(Archive & ar, const unsigned int version)
            {
                ar & boost::serialization::base_object<Human>(*this);
                ar & number;
                ar & idnumber;
            }
        };

        PlayerPosition IntToPlayerPosition(int i);
    }
}

#endif // PLAYER_H
