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
  This file was generated on So Okt 26 2008 at 12:09:20
**************************************************************************/

#include "Input.h"

namespace freekick
{
    namespace match
    {
        namespace client
        {
            namespace cl_ogre
            {
                Input::Input (Configuration* conf, MatchStatus* stat, Network* netw ) 
                    : configuration(conf), 
                      status(stat), 
                      network(netw)
                {
                }

                Input::~Input()
                {
                    if (inputhandler) delete inputhandler;
                }

                boost::shared_ptr<InputConfiguration>& Input::getInputConfiguration ( ) const 
                {
                    return configuration->getInputConfiguration();
                }

                void Input::setupInputSystem(const std::string& windowhnd, 
                                             unsigned int width, 
                                             unsigned int height, 
                                             Ogre::SceneManager* mgr,
                                             Ogre::Camera* cam)
                {
                    inputhandler = new InputHandler(configuration->getInputConfiguration(), windowhnd, width, height, network, mgr, cam);
                }

                Ogre::FrameListener* Input::getFrameListener()
                {
                    return inputhandler;
                }
            }
        }
    }
}
