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


#ifndef CONSOLE_H
#define CONSOLE_H

#include <boost/shared_ptr.hpp>

#include "Physics.h"
#include "Rules.h"
#include "ConsoleCommand.h"

namespace freekick
{
    namespace match
    {
        namespace server
        {
            class Console
            {
            public:
                Console ( );
                virtual ~Console ( );
                Console (Physics physics, Rules rules);
                void run ( );

            protected:
                void getInput (ConsoleCommand cc ) const;

            private:
                bool mContinue;
                boost::shared_ptr<Physics> mPhysics;
                boost::shared_ptr<Rules> mRules;

            public:
                void setContinue ( bool new_var );
                bool getContinue ( );
                void setPhysics ( boost::shared_ptr<Physics> new_var );
                boost::shared_ptr<Physics> getPhysics ( );
                void setRules ( boost::shared_ptr<Rules> new_var );
                boost::shared_ptr<Rules> getRules ( );
            };
        }
    }
}

#endif // CONSOLE_H
