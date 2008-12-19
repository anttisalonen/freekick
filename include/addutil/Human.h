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


#ifndef HUMAN_H
#define HUMAN_H

#include <string>

#include <boost/serialization/string.hpp>

#include "Color.h"

/**
 * class Human
 */

namespace addutil
{
    class Human
    {
    public:
        virtual ~Human() { }
        virtual const std::string& getName();
    protected:
        Human(const std::string& n);

    private:

        std::string name;
        Color skincolor;
        Color haircolor;

        friend class boost::serialization::access;
        template<class Archive>
            void serialize(Archive & ar, const unsigned int version)
        {
            ar & name;
            ar & skincolor;
            ar & haircolor;
        }
    };
}

#endif // HUMAN_H
