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


#ifndef MOUSEINPUTTYPE_H
#define MOUSEINPUTTYPE_H

#include <OIS/OIS.h>

namespace freekick
{
    namespace match
    {
        namespace client
        {
            namespace cl_ogre
            {
                class MouseInputType : public InputType
                {
                public:
                    MouseInputType(OIS::MouseButtonID _id);
                    InputType getInputType();

                private:
                    OIS::MouseButtonID id;

                };
            }
        }
    }
}

#endif // INPUTTYPE_H
