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

#include "Console.h"

// Constructors/Destructors
//  

namespace freekick
{
    namespace match
    {
        namespace server
        {
            Console::Console ( ) {
            }

            Console::~Console ( ) { }

            void Console::setContinue ( bool new_var ) {
                mContinue = new_var;
            }

/**
 * Get the value of mContinue
 * @return the value of mContinue
 */
            bool Console::getContinue ( ) {
                return mContinue;
            }

/**
 * Set the value of mPhysics
 * @param new_var the new value of mPhysics
 */
            void Console::setPhysics ( boost::shared_ptr<Physics> new_var ) {
                mPhysics = new_var;
            }

/**
 * Get the value of mPhysics
 * @return the value of mPhysics
 */
            boost::shared_ptr<Physics> Console::getPhysics ( ) {
                return mPhysics;
            }

/**
 * Set the value of mRules
 * @param new_var the new value of mRules
 */
            void Console::setRules ( boost::shared_ptr<Rules> new_var ) {
                mRules = new_var;
            }

/**
 * Get the value of mRules
 * @return the value of mRules
 */
            boost::shared_ptr<Rules> Console::getRules ( ) {
                return mRules;
            }

/**
 * @param  physics
 * @param  rules
 * @param  connectionlistener
 */
            Console::Console (Physics physics, Rules rules)
            {

            }


/**
 */
            void Console::run ( ) {

            }


/**
 * @param  cc
 */
            void Console::getInput (ConsoleCommand cc ) const {

            }
        }
    }
}


