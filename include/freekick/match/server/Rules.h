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


#ifndef RULES_H
#define RULES_H

#include <boost/shared_ptr.hpp>

#include "RulesState.h"
#include "PhysicsEvent.h"
#include "Dispatcher.h"
#include "RulesEvent.h"



namespace freekick
{
    namespace match
    {
        namespace server
        {
            class Rules
            {
            public:
                Rules ( );
                virtual ~Rules ( );
                Rules (RulesState rulesstatus);
                void update (PhysicsEventList pes );
                void createRulesEvents (/*Time t, */ PhysicsEventList pes, RulesEventList res );

            private:

                // Private attributes
                //  

                boost::shared_ptr<RulesState> mRulesState;
                boost::shared_ptr<Dispatcher> mDispatcher;
            public:
                void setRulesState ( boost::shared_ptr<RulesState> new_var );
                boost::shared_ptr<RulesState> getRulesState ( );
                void setDispatcher ( boost::shared_ptr<Dispatcher> new_var );
                boost::shared_ptr<Dispatcher> getDispatcher ( );
            };
        }
    }
}
#endif // RULES_H
