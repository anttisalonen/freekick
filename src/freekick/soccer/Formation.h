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


#ifndef FREEKICKSOCCERFORMATION_H
#define FREEKICKSOCCERFORMATION_H

#include <map>
#include <iostream>
#include <string>

#include <boost/shared_ptr.hpp>

#include "addutil/Square.h"
#include "addutil/XML.h"
#include "addutil/Exception.h"

#include "Tactic.h"
#include "GeneralTactic.h"
#include "Lineup.h"

namespace freekick
{
    namespace soccer
    {
        typedef std::map<const std::string, boost::shared_ptr<Tactic> > TacticList;
        class Formation
        {
        public:
            Formation();
            Formation(xmlNodePtr root);
            virtual ~Formation() { }
            addutil::Vector3 getPitchPoint(const std::string& pos) const;
            boost::shared_ptr<Lineup> getLineup() const;
            void setLineup(boost::shared_ptr<Lineup> l);
        private:
            TacticList mTacticList;
            boost::shared_ptr<Lineup> mLineup;
            boost::shared_ptr<GeneralTactic> mGeneralTactic;
        };
    }
}

#endif
