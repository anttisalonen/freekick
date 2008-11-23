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





namespace freekick
{
    namespace match
    {
        namespace server
        {
            class Rules
            {
            public:

                // Constructors/Destructors
                //  


                /**
                 * Empty Constructor
                 */
                Rules ( );

                /**
                 * Empty Destructor
                 */

                virtual ~Rules ( );



                /**
                 * @param  rulesstatus
                 * @param  ioservice
                 */
                Rules (freekick::match::RulesState rulesstatus, io_service ioservice );


                /**
                 * @param  pes
                 */
                void update (freekick::match::PhysicsEventList pes );


                /**
                 * @param  t
                 * @param  pes
                 * @param  res
                 */
                void createRulesEvents (Time t, freekick::match::PhysicsEventList pes, freekick::match::RulesEventList res );

            protected:

            public:

            protected:

            public:

            protected:


            private:

                // Private attributes
                //  

                boost::shared_ptr<RulesState> mRulesState;
                boost::shared_ptr<Dispatcher> mDispatcher;
            public:

            private:

            public:


                // Private attribute accessor methods
                //  


                /**
                 * Set the value of mRulesState
                 * @param new_var the new value of mRulesState
                 */
                void setMRulesState ( boost::shared_ptr<RulesState> new_var );

                /**
                 * Get the value of mRulesState
                 * @return the value of mRulesState
                 */
                boost::shared_ptr<RulesState> getMRulesState ( );


                /**
                 * Set the value of mDispatcher
                 * @param new_var the new value of mDispatcher
                 */
                void setMDispatcher ( boost::shared_ptr<Dispatcher> new_var );

                /**
                 * Get the value of mDispatcher
                 * @return the value of mDispatcher
                 */
                boost::shared_ptr<Dispatcher> getMDispatcher ( );

            private:


                void initAttributes ( ) ;

            };
        }
    }
}
#endif // RULES_H
