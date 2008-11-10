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


#ifndef GRAPHICSUPDATER_H
#define GRAPHICSUPDATER_H

#include <iostream>
#include <vector>
#include <map>

#include <boost/foreach.hpp>
#include <boost/shared_ptr.hpp>

#include <Ogre.h>

#include "Status.h"
#include "Color.h"

/**
 * class GraphicsUpdater
 */

class GraphicsUpdater : public Ogre::FrameListener
{
public:
    GraphicsUpdater(Status* s);
    virtual ~GraphicsUpdater();
    bool frameStarted(const Ogre::FrameEvent& evt);
    void setSceneManager(Ogre::SceneManager* s);

private:
    Status* status;
    Ogre::SceneManager* smgr;
    std::map <int, Ogre::Entity* > entitymap;
};

#endif // GRAPHICSUPDATER_H
