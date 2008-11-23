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
            Rules::Rules ( ) {
                initAttributes();
            }

            Rules::~Rules ( ) { }

//  
// Methods
//  


// Accessor methods
//  


// Private attribute accessor methods
//  


/**
 * Set the value of mRulesState
 * @param new_var the new value of mRulesState
 */
            void Rules::setMRulesState ( boost::shared_ptr<RulesState> new_var ) {
                mRulesState = new_var;
            }

/**
 * Get the value of mRulesState
 * @return the value of mRulesState
 */
            boost::shared_ptr<RulesState> Rules::getMRulesState ( ) {
                return mRulesState;
            }

/**
 * Set the value of mDispatcher
 * @param new_var the new value of mDispatcher
 */
            void Rules::setMDispatcher ( boost::shared_ptr<Dispatcher> new_var ) {
                mDispatcher = new_var;
            }

/**
 * Get the value of mDispatcher
 * @return the value of mDispatcher
 */
            boost::shared_ptr<Dispatcher> Rules::getMDispatcher ( ) {
                return mDispatcher;
            }

// Other methods
//  


/**
 * @param  rulesstatus
 * @param  ioservice
 */
            Rules::Rules (freekick::match::RulesState rulesstatus, io_service ioservice ) {

            }


/**
 * @param  pes
 */
            void Rules::update (freekick::match::PhysicsEventList pes ) {

            }


/**
 * @param  t
 * @param  pes
 * @param  res
 */
            void Rules::createRulesEvents (Time t, freekick::match::PhysicsEventList pes, freekick::match::RulesEventList res ) {

            }

            void Rules::initAttributes ( ) {
            }
        }
    }
}

