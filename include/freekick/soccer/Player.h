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

#include "Human.h"
#include "Color.h"

/**
 * class Player
 */

namespace freekick
{
    namespace soccer
    {
        class Player : public addutil::Human
        {
        public:

            // Constructors/Destructors
            //  

            /**
             * @param  name
             * @param  num
             */
            Player (const std::string& _name, unsigned int num, unsigned int _idnumber);
            const int getID();

        private:

            const unsigned int number;
            const unsigned int idnumber;

        };
    }
}

#endif // PLAYER_H
