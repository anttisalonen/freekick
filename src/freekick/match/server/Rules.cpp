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
  This file was generated on Sa Nov 22 2008 at 11:40:55
**************************************************************************/

#include "Rules.h"

// Constructors/Destructors
//  

namespace freekick
{
    namespace match
    {
        namespace server
        {
            Rules::Rules (boost::shared_ptr<Dispatcher> d, boost::shared_ptr<MatchStatus> ms)
                : mDispatcher(d)
                , mMatchStatus(ms)
            {
            }

            Rules::~Rules ( ) { }

            void Rules::setRulesState ( boost::shared_ptr<RulesState> new_var ) 
            {
                mRulesState = new_var;
            }

            boost::shared_ptr<RulesState> Rules::getRulesState ( ) 
            {
                return mRulesState;
            }

            void Rules::update (PhysicsEventList pes ) 
            {

            }

            void Rules::createRulesEvents (Time t, PhysicsEventList pes, RulesEventList res ) 
            {

            }
        }
    }
}

