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


#ifndef GRAPHICS_H
#define GRAPHICS_H

#include <iostream>
#include <map>

#include <boost/shared_ptr.hpp>

#include <Ogre.h>
#include <CEGUI/CEGUI.h>
#include <OgreCEGUIRenderer.h>

#include "Configuration.h"
#include "MatchStatus.h"
#include "Input.h"
#include "GraphicsUpdater.h"

/**
 * class Graphics
 */

namespace freekick
{
    namespace match
    {
        namespace client
        {
            namespace cl_ogre
            {
                class Graphics
                {
                public:

                    /**
                     * @param  conf
                     * @param  stat
                     * @param  inp
                     */
                    Graphics (Configuration* conf, MatchStatus* stat, Input* inp);
                    virtual ~Graphics();
                    bool run();

                    /**
                     * @return bool
                     */
                    bool draw ( );

                private:
                    void createRoot();
                    void defineResources();
                    void setupRenderSystem();
                    void createRenderWindow();
                    void initializeResourceGroups();
                    void setupScene();
                    void setupInputSystem();
                    void setupCEGUI();
                    void startRenderLoop();

                    Configuration* configuration;
                    MatchStatus* status;
                    Input* input;
                    GraphicsUpdater* updater;

                    Ogre::Root *mRoot;
                    CEGUI::OgreCEGUIRenderer *mRenderer;
                    CEGUI::System *mSystem;

                };
            }
        }
    }
}

#endif // GRAPHICS_H
