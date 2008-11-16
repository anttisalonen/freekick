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


#ifndef INPUT_H
#define INPUT_H

#include <string>

#include <boost/shared_ptr.hpp>

#include <Ogre.h>

#include <OIS/OIS.h>

#include "Configuration.h"
#include "Status.h"
#include "Network.h"
#include "InputHandler.h"
#include "Entity.h"

/**
 * class Input
 */

namespace freekick
{
    namespace match
    {
        namespace client
        {
            namespace cl_ogre
            {
                using namespace freekick::match::network;
                class Input
                {
                public:

                    Input (Configuration* conf, Status* stat, Network* netw );
                    ~Input();
                    InputConfiguration* getInputConfiguration ( ) const;
                    void setCameraPos(float x, float y, float z);
                    void setupInputSystem(const std::string& windowhnd, unsigned int width, unsigned int height, Ogre::SceneNode* n);
                    void setCamera(Ogre::SceneNode* n);
                    Ogre::FrameListener* getFrameListener();

                private:

                    Configuration* configuration;
                    Status* status;
                    Network* network;
                    InputHandler* inputhandler;

                };
            }
        }
    }
}

#endif // INPUT_H
