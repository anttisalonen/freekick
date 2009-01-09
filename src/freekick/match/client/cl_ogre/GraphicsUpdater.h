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


#ifndef GRAPHICSUPDATER_H
#define GRAPHICSUPDATER_H

#include <iostream>
#include <vector>
#include <map>

#include <boost/foreach.hpp>
#include <boost/shared_ptr.hpp>

#include <Ogre.h>

#include "addutil/Color.h"
#include "addutil/DynamicEntity.h"

#include "MatchStatus.h"
#include "MatchPlayer.h"

/**
 * class GraphicsUpdater
 */

namespace freekick
{
    namespace match
    {
        namespace client
        {
            namespace cl_ogre
            {
                class GraphicsUpdater : public Ogre::FrameListener
                {
                public:
                    GraphicsUpdater(MatchStatus* s);
                    virtual ~GraphicsUpdater();
                    bool frameStarted(const Ogre::FrameEvent& evt);
                    void setSceneManager(Ogre::SceneManager* s);

                private:
                    MatchStatus* status;
                    Ogre::SceneManager* smgr;
                    std::map <int, Ogre::Entity* > entitymap;

                    bool updateOgreNode(boost::shared_ptr<DynamicEntity> d);
                    bool updateOgreNode(std::pair<const int, boost::shared_ptr<MatchPlayer> > d);
                };
            }
        }
    }
}

#endif // GRAPHICSUPDATER_H
