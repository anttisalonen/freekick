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


#ifndef BALL_H
#define BALL_H

#include <string>

#include <boost/serialization/serialization.hpp>
#include <boost/serialization/string.hpp>

/**
 * class Ball
 */

namespace freekick
{
    namespace soccer
    {
        class Ball
        {
        public:
            Ball(float m);
            float getMass() const;
        private:
            std::string texture;
            float mass;

            friend class boost::serialization::access;
            template<class Archive>
                void serialize(Archive & ar, const unsigned int version)
            {
                ar & texture;
                ar & mass;
            }
        };
    }
}
    
#endif // BALL_H
