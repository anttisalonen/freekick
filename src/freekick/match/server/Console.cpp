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
  This file was generated on Sa Nov 22 2008 at 11:40:55
**************************************************************************/

#include "Console.h"

// Constructors/Destructors
//  

namespace freekick
{
    namespace match
    {
        namespace server
        {
            Console::~Console ( ) { }

            Console::Console (boost::shared_ptr<MatchStatus> ms)
                : mMatchStatus(ms)
            {
            }

            void Console::run ( ) 
            {
                using namespace std;
                bool quit = false;
                while(!quit)
                {
                    string command;
                    cin >> command;
                    if(command == "ball")
                    {
                        cout << *(mMatchStatus->getBall()) << endl;
                    }
                    else if(command == "state")
                    {
                        cout << mMatchStatus->getBallState() << endl;
                    }
                    else if(command == "quit")
                    {
                        mMatchStatus->setContinue(false);
                        quit = true;
                    }
                }
            }
        }
    }
}


