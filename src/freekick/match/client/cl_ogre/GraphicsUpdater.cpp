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
                    typedef boost::shared_ptr<MatchPlayer> PlayerPtr;
                    std::map <int, PlayerPtr> drs;
                    status->getPlayers(drs);
                    std::map <int, PlayerPtr>::iterator d;

                    bool success;

                    // Players
                    for (d = drs.begin(); d != drs.end(); d++)
                    {
                        success = updateOgreNode(*d, evt);
                        if(!success) return false;
                    }

                    // Ball
                    boost::shared_ptr<MatchBall> b = status->getBall();
                    success = updateOgreNode(b, evt);

                    return success;
                }

                bool GraphicsUpdater::updateOgreNode(boost::shared_ptr<DynamicEntity> d, const Ogre::FrameEvent& evt)
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
                            node = smgr->getRootSceneNode()->createChildSceneNode(nname.str());
                            node->attachObject(ent);
                            if(dr_id == BallID)
                            {
                                node->setScale(0.3f, 0.3f, 0.3f);
                            }
                            else if(dr_id > 0)  // Player
                            {
                                node->setScale(0.03f, 0.03f, 0.03f);
                                soccer::BallOwner bo = status->getPlayerSide(dr_id);
                                if(bo == Away)       // TODO: get actual kits
                                {
                                    ent->setMaterialName("Examples/EnvMappedRustySteel");
                                }
                            }
                            entitymap[dr_id] = ent;
                        }
                        else
                            return false;
                    }

                    if(node)
                    {
                        const Vector3& pos = d->getPosition();
                        node->setPosition(Ogre::Vector3(pos.x, pos.y, pos.z));
                        const Quaternion& dir = d->getOrientation();
                        node->setOrientation(Ogre::Quaternion(dir.w, dir.x, dir.y, dir.z));
                        if(it != entitymap.end())
                        {
                            if(d->getModel() == "robot.mesh")
                            {
                                const Vector3& vel = d->getVelocity();
                                Ogre::AnimationState* as;
                                if(vel.length() > 0.3f)
                                {
                                    as = it->second->getAnimationState("Walk");
                                }
                                else
                                {
                                    as = it->second->getAnimationState("Idle");
                                }
                                if(as)
                                {
                                    as->setLoop(true);
                                    as->setEnabled(true);
                                    as->addTime(evt.timeSinceLastFrame * vel.length() * 0.35f);
                                }
                                else
                                {
                                    std::cerr << "Error setting animation.\n";
                                }
                            }
                        }
                    }
                    return true;
                }

                bool GraphicsUpdater::updateOgreNode(std::pair<const int, boost::shared_ptr<MatchPlayer> > d, const Ogre::FrameEvent& evt)
                {
                    return updateOgreNode(d.second, evt);
                }

                void GraphicsUpdater::setSceneManager(Ogre::SceneManager* s)
                {
                    smgr = s;
                }
            }
        }
    }
}
