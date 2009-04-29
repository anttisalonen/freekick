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


#ifndef MATCHPLAYER_H
#define MATCHPLAYER_H

#include <string>

#include <boost/serialization/base_object.hpp>

#include "addutil/DynamicEntity.h"
#include "addutil/Color.h"

#include "Player.h"
#include "Primitives.h"

/**
 * class Player
 */

namespace freekick
{
    namespace match
    {
        class MatchPlayer : public freekick::soccer::Player, public addutil::DynamicEntity
        {
        public:

            // Constructors/Destructors
            //  
            MatchPlayer (const freekick::soccer::Player& p, soccer::BallOwner _side, bool sub);
            int getID() const;
            bool isSubstitute() const;
            soccer::BallOwner getSide() const;
            void updateTimer(float time_interval);
            void setKickTimer(float kicktime);
            bool timerCanKick() const;

        private:
            soccer::BallOwner m_side;
            bool substitute;
            float m_kicktimer;

            friend class boost::serialization::access;
            template<class Archive>
                void serialize(Archive & ar, const unsigned int version)
            {
                ar & boost::serialization::base_object<freekick::soccer::Player>(*this);
                ar & boost::serialization::base_object<addutil::DynamicEntity>(*this);
                ar & m_side;
                ar & substitute;
                ar & m_kicktimer;
            }
        };
    }
}

#endif // MATCHPLAYER_H
