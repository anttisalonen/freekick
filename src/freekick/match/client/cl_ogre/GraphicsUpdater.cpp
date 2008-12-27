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

#include "GraphicsUpdater.h"

namespace freekick
{
    namespace match
    {
        namespace client
        {
            namespace cl_ogre
            {
                GraphicsUpdater::GraphicsUpdater(MatchStatus* s)
                    : status(s)
                {
                }

                GraphicsUpdater::~GraphicsUpdater()
                {
                }

                bool GraphicsUpdater::frameStarted(const Ogre::FrameEvent& evt)
                {
                    typedef boost::shared_ptr<MatchPlayer> PtrPlayer;
                    std::map <int, PtrPlayer> drs;
                    status->getPlayers(drs);
                    std::map <int, PtrPlayer>::iterator d;

                    bool success;
                    for (d = drs.begin(); d != drs.end(); d++)
                    {
                        success = updateOgreNode(*d);
                        if(!success) return false;
                    }

                    boost::shared_ptr<MatchBall> b = status->getBall();
                    success = updateOgreNode(b);

                    return success;
                }

                bool GraphicsUpdater::updateOgreNode(boost::shared_ptr<DynamicEntity> d)
                {
                    Ogre::SceneNode* node = 0;
                    int dr_id = d->getID();
                    std::map <int, Ogre::Entity* >::iterator it = entitymap.find(dr_id);
                    if(it != entitymap.end())
                    {
                        node = it->second->getParentSceneNode();
                    }
                    else
                    {
                        if(smgr)
                        {
                            std::stringstream ename;
                            ename << "Entity" << dr_id;
                            std::stringstream nname;
                            nname << "Node" << dr_id;
                            const std::string modelfile = d->getModel();
                            Ogre::Entity* ent = smgr->createEntity (ename.str(), modelfile);
                            ent->setCastShadows(true);
                            if(dr_id >= 200)        // TODO: get actual kits
                                ent->setMaterialName("Examples/EnvMappedRustySteel");
                            node = smgr->getRootSceneNode()->createChildSceneNode(nname.str());
                            node->attachObject(ent);
                            if(modelfile == "robot.mesh")
                                node->setScale(0.05f, 0.05f, 0.05f);
                            entitymap[dr_id] = ent;
                        }
                        else
                            return false;
                    }

                    if(node)
                    {
                        const Vector3& pos = d->getPosition();
                        node->setPosition(Ogre::Vector3(-pos.x, pos.y, pos.z));
                        const Quaternion& dir = d->getOrientation();
                        node->setOrientation(Ogre::Quaternion(dir.w, dir.x, dir.y, dir.z));
                    }
                    return true;
                }

                bool GraphicsUpdater::updateOgreNode(std::pair<const int, boost::shared_ptr<MatchPlayer> > d)
                {
                    return updateOgreNode(d.second);
                }

                void GraphicsUpdater::setSceneManager(Ogre::SceneManager* s)
                {
                    smgr = s;
                }
            }
        }
    }
}
