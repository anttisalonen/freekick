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

#include "InputHandler.h"

namespace freekick
{
    namespace match
    {
        namespace client
        {
            namespace cl_ogre
            {
                InputHandler::InputHandler (int player_id,
                                            bool follow_mouse,
                                            const boost::shared_ptr<InputConfiguration>& inputconf, 
                                            MatchStatus* ms,
                                            const std::string& windowhnd, 
                                            Ogre::RenderWindow* win,
                                            Network* netw,
                                            Ogre::SceneManager* mgr,
                                            Ogre::Camera* cam)
                    : inputconfiguration(inputconf),
                      mMatchStatus(ms),
                      network(netw),
                      mControlledPlayer(player_id),
                      mSceneMgr(mgr),
                      mCamera(cam),
                      mRenderWindow(win),
                      mFollowMouse(follow_mouse)
                {
                    mRotate = 0.13;
                    mMove = 50;
                    mDirection = Ogre::Vector3::ZERO;
                    mContinue = true;

                    OIS::ParamList pl;
                    pl.insert(std::make_pair(std::string("WINDOW"), windowhnd));

#if defined OIS_WIN32_PLATFORM
                    pl.insert(std::make_pair(std::string("w32_mouse"), std::string("DISCL_FOREGROUND" )));
                    pl.insert(std::make_pair(std::string("w32_mouse"), std::string("DISCL_NONEXCLUSIVE")));
                    pl.insert(std::make_pair(std::string("w32_keyboard"), std::string("DISCL_FOREGROUND")));
                    // pl.insert(std::make_pair(std::string("w32_keyboard"), std::string("DISCL_NONEXCLUSIVE")));
#elif defined OIS_LINUX_PLATFORM
                    pl.insert(std::make_pair(std::string("x11_mouse_grab"), std::string("false")));
                    pl.insert(std::make_pair(std::string("x11_mouse_hide"), std::string("false")));
                    pl.insert(std::make_pair(std::string("x11_keyboard_grab"), std::string("false")));
                    // pl.insert(std::make_pair(std::string("XAutoRepeatOn"), std::string("true")));
#endif

                    mInputManager = OIS::InputManager::createInputSystem(pl);

                    try
                    {
                        mKeyboard = static_cast<OIS::Keyboard*>(mInputManager->createInputObject(OIS::OISKeyboard, true));
                        mMouse = static_cast<OIS::Mouse*>(mInputManager->createInputObject(OIS::OISMouse, true));
                        //mJoy = static_cast<OIS::JoyStick*>(mInputManager->createInputObject(OIS::OISJoyStick, true));
                        const OIS::MouseState &ms = mMouse->getMouseState();

                        unsigned int width, height, depth;
                        int top, left;
                        mRenderWindow->getMetrics(width, height, depth, left, top);

                        ms.width = width;
                        ms.height = height;
                        mMouse->setEventCallback(this);
                        mKeyboard->setEventCallback(this);
                        Ogre::WindowEventUtilities::addWindowEventListener(mRenderWindow, this);
                    }
                    catch (const OIS::Exception& e)
                    {
                        std::cerr << "Error while initialising keyboard and mouse" << std::endl;
                        throw Ogre::Exception(42, e.eText, "InputHandler");
                    }

                    // Camera
                    mCamNodes[0] = mSceneMgr->getRootSceneNode()->createChildSceneNode("CamNode0");
                    mCamNodes[1] = mSceneMgr->getRootSceneNode()->createChildSceneNode("CamNode1");
                    mCamNodes[2] = mSceneMgr->getRootSceneNode()->createChildSceneNode("CamNode2");
                    init_camera_nodes();

                    mCamNodes[1]->attachObject(mCamera);
                    mCamNode = mCamNodes[1];
                    mCamMode = CamLengthFar;

                    mRaySceneQuery = mSceneMgr->createRayQuery(Ogre::Ray());
                    // mRaySceneQuery->setWorldFragmentType(Ogre::SceneQuery::WFT_SINGLE_INTERSECTION);

                    if(mControlledPlayer > 0)
                        network->sendMessage(messages::PlayerControlRequestMessage(mControlledPlayer));
                }

                InputHandler::~InputHandler()
                {
                    mSceneMgr->destroyQuery(mRaySceneQuery);

                    if(mKeyboard) mInputManager->destroyInputObject(mKeyboard);
                    if(mMouse) mInputManager->destroyInputObject(mMouse);
                    OIS::InputManager::destroyInputSystem(mInputManager);
                }

                void InputHandler::init_camera_nodes()
                {
                    // TODO: read pitch dimensions from matchstatus
                    mCamNodes[0]->setPosition (Ogre::Vector3(35.0f, 100.0f, 50.0f));
                    mCamNodes[0]->lookAt (Ogre::Vector3(35.0f, 0, 50.0f), Ogre::Node::TS_WORLD);
                    mCamNodes[0]->roll(Ogre::Radian(Ogre::Math::HALF_PI));

                    mCamNodes[1]->setPosition (Ogre::Vector3(35.0f, 75.0f, 120.0f));
                    mCamNodes[1]->lookAt (Ogre::Vector3(35.0f, 0, 65.0f), Ogre::Node::TS_WORLD);

                    mCamNodes[2]->setPosition (Ogre::Vector3(105.0f, 45.0f, 50.0f));
                    mCamNodes[2]->lookAt (Ogre::Vector3(35.0f, 0, 50.0f), Ogre::Node::TS_WORLD);
                }

                bool InputHandler::frameStarted(const Ogre::FrameEvent& evt)
                {
                    if (mKeyboard)
                        mKeyboard->capture();
                    if (mMouse)
                        mMouse->capture();
                    if (mCamNode)
                        mCamNode->translate(mDirection * evt.timeSinceLastFrame, Ogre::Node::TS_LOCAL);

                    return mContinue;
                }

                Ogre::Vector3 InputHandler::getPointedPitchPosition(const OIS::MouseEvent& e) const
                {
                    using namespace Ogre;
                    // CEGUI::Point mousePos = CEGUI::MouseCursor::getSingleton().getPosition();
                    Ray mouseRay = mCamera->getCameraToViewportRay(
                        e.state.X.abs / float(e.state.width), 
                        e.state.Y.abs / float(e.state.height));
                    mRaySceneQuery->setRay(mouseRay);
                    mRaySceneQuery->setSortByDistance(false);
                    // std::cerr << "X: " << e.state.X.abs << "; Y: " << e.state.Y.abs << 
                    // "; width: " << e.state.width << "; height: " << e.state.height << std::endl;
                    RaySceneQueryResult& result = mRaySceneQuery->execute();
                    RaySceneQueryResult::iterator itr = result.begin();

                    for(itr = result.begin(); itr != result.end(); itr++)
                    {
                        if(itr->distance == 0.0f)
                            continue;
                        return mouseRay.getPoint(itr->distance);
                    }
                    return Ogre::Vector3(0.0f, 0.0f, 0.0f);
                }

                bool InputHandler::mouseReleased (const OIS::MouseEvent& e, OIS::MouseButtonID id )
                {
                    if(mControlledPlayer < 1)
                        return true;

                    Ogre::Vector3 clickpos = getPointedPitchPosition(e);
                    if(clickpos.x == 0.0f && clickpos.y == 0 && clickpos.z == 0)
                        return true;
                    // std::cerr << "Clicked; dist: " << itr->distance << "; pos: " << clickpos.x << ";" << clickpos.y << ";" << clickpos.z << std::endl;
                    try
                    {
                        boost::shared_ptr<MatchPlayer> pl = mMatchStatus->getPlayer(mControlledPlayer);
                        addutil::Vector3 plpos = pl->getPosition();
                        addutil::Vector3 target(clickpos.x - plpos.x, clickpos.y - plpos.y, clickpos.z - plpos.z);
                        if(id == OIS::MB_Left)
                        {
                            Helpers::correctPassVector(target);
                        }
                        else
                        {
                            Helpers::correctShootVector(target);
                        }
                        network->sendMessage(messages::KickPlayerControlMessage(mControlledPlayer, target));
                    }
                    catch(...)
                    {
                        std::cerr << "Tried to kick, but the player is not in the match status.\n";
                    }

                    return true;
                }

                bool InputHandler::mousePressed (const OIS::MouseEvent& e, OIS::MouseButtonID id )
                {
                    return true;
                }

                bool InputHandler::mouseMoved (const OIS::MouseEvent& e )
                {
                    if(!mFollowMouse)
                        return true;

                    if(mControlledPlayer < 1)
                        return true;

                    if(e.state.buttonDown(OIS::MB_Left) || e.state.buttonDown(OIS::MB_Right))
                        return true;     // do not update when holding button down

                    Ogre::Vector3 clickpos = getPointedPitchPosition(e);
                    if(clickpos.x == 0.0f && clickpos.y == 0 && clickpos.z == 0)
                        return true;
                    clickpos.y = 0.0f;
                    try
                    {
                        boost::shared_ptr<MatchPlayer> pl = mMatchStatus->getPlayer(mControlledPlayer);
                        addutil::Vector3 plpos = pl->getPosition();
                        addutil::Vector3 target(clickpos.x - plpos.x, clickpos.y - plpos.y, clickpos.z - plpos.z);
                        network->sendMessage(messages::MovePlayerControlMessage(mControlledPlayer, target));
                    }
                    catch(...)
                    {
                        std::cerr << "Tried to move, but the player is not in the match status.\n";
                    }

                    return true;
                }

                bool InputHandler::keyPressed (const OIS::KeyEvent& e )
                {
                    bool sendmove = false;
                    switch (e.key)
                    {
                        case OIS::KC_ESCAPE: 
                            mContinue = false;
                            break;

                        case OIS::KC_UP:
                            mDirection.z -= mMove;
                            break;

                        case OIS::KC_DOWN:
                            mDirection.z += mMove;
                            break;

                        case OIS::KC_LEFT:
                            mDirection.x -= mMove;
                            break;

                        case OIS::KC_RIGHT:
                            mDirection.x += mMove;
                            break;

                        case OIS::KC_PGDOWN:
                            mDirection.y -= mMove;
                            break;

                        case OIS::KC_PGUP:
                            mDirection.y += mMove;
                            break;

                        case OIS::KC_O:
                        {
                            const Ogre::Vector3& vec = mCamera->getPosition();
                            std::cerr << "Camera position: " << vec.x << "; " << vec.y << "; " << vec.z << std::endl;
                            const Ogre::Quaternion& q = mCamera->getOrientation();
                            std::cerr << "Camera orientation: " << q.w << "; " << q.x << "; " << q.y << "; " << q.z << std::endl;
                            break;
                        }

                        case OIS::KC_W:
                            setMoveVector(DirUp, 15.0f);
                            sendmove = true;
                            break;

                        case OIS::KC_S:
                            setMoveVector(DirDown, 15.0f);
                            sendmove = true;
                            break;

                        case OIS::KC_A:
                            setMoveVector(DirLeft, 15.0f);
                            sendmove = true;
                            break;

                        case OIS::KC_D:
                            setMoveVector(DirRight, 15.0f);
                            sendmove = true;
                            break;

                        case OIS::KC_Q:
                            setMoveVector(DirJump, 15.0f);
                            sendmove = true;
                            break;

                        case OIS::KC_E:
                            setMoveVector(DirCrouch, 15.0f);
                            sendmove = true;
                            break;

                        case OIS::KC_X:
                            if(mControlledPlayer > 0)
                                network->sendMessage(messages::PlayerControlRequestMessage(mControlledPlayer));
                            break;

                        case OIS::KC_1:
                            switchToCameraMode(CamSky);
                            break;

                        case OIS::KC_2:
                            switchToCameraMode(CamLengthFar);
                            break;

                        case OIS::KC_3:
                            switchToCameraMode(CamTV);
                            break;

                        default:
                            break;
                    }
                    if(sendmove) 
                    {
                        sendMoveMessage(mRunDirection);
                    }
                    return true;
                }

                bool InputHandler::keyReleased (const OIS::KeyEvent& e )
                {
                    bool sendmove = false;
                    switch (e.key)
                    {
                        case OIS::KC_W:
                        case OIS::KC_UP:
                            setMoveVector(DirUp, -15.0f);
                            sendmove = true;
                            break;

                        case OIS::KC_S:
                        case OIS::KC_DOWN:
                            setMoveVector(DirDown, -15.0f);
                            sendmove = true;
                            break;

                        case OIS::KC_A:
                        case OIS::KC_LEFT:
                            setMoveVector(DirLeft, -15.0f);
                            sendmove = true;
                            break;

                        case OIS::KC_D:
                        case OIS::KC_RIGHT:
                            setMoveVector(DirRight, -15.0f);
                            sendmove = true;
                            break;

                        case OIS::KC_Q:
                        case OIS::KC_PGUP:
                            setMoveVector(DirUp, -15.0f);
                            sendmove = true;
                            break;

                        case OIS::KC_E:
                        case OIS::KC_PGDOWN:
                            setMoveVector(DirCrouch, -15.0f);
                            sendmove = true;
                            break;

                        case OIS::KC_RCONTROL:
                        case OIS::KC_LCONTROL:
                        {
                            addutil::Vector3 target(mRunDirection);
                            target.normalize();
                            target *= 20.0f;
                            Helpers::correctPassVector(target);
                            network->sendMessage(messages::KickPlayerControlMessage(mControlledPlayer, target));
                            break;
                        }

                        case OIS::KC_RSHIFT:
                        case OIS::KC_LSHIFT:
                        {
                            addutil::Vector3 target(mRunDirection);
                            target.normalize();
                            target *= 20.0f;
                            Helpers::correctShootVector(target);
                            network->sendMessage(messages::KickPlayerControlMessage(mControlledPlayer, target));
                            break;
                        }

                        default:
                            break;
                    } // switch
                    if(sendmove) 
                    {
                        sendMoveMessage(mRunDirection);
                    }
                    return true;
                }

                void InputHandler::windowResized(Ogre::RenderWindow* rw)
                {
                    const OIS::MouseState &ms = mMouse->getMouseState();
                    ms.width = rw->getWidth();
                    ms.height = rw->getHeight();
                }

                void InputHandler::setMoveVector(SubjectiveDirection d, float mag)
                {
                    switch(d)
                    {
                        case DirUp:
                            if(mCamMode == CamSky || mCamMode == CamTV)
                                mRunDirection.x -= mag;
                            else if (mCamMode == CamLengthFar)
                                mRunDirection.z -= mag;
                            break;
                        case DirDown:
                            if(mCamMode == CamSky || mCamMode == CamTV)
                                mRunDirection.x += mag;
                            else if (mCamMode == CamLengthFar)
                                mRunDirection.z += mag;
                            break;
                        case DirLeft:
                            if(mCamMode == CamSky || mCamMode == CamTV)
                                mRunDirection.z += mag;
                            else if (mCamMode == CamLengthFar)
                                mRunDirection.x -= mag;
                            break;
                        case DirRight:
                            if(mCamMode == CamSky || mCamMode == CamTV)
                                mRunDirection.z -= mag;
                            else if (mCamMode == CamLengthFar)
                                mRunDirection.x += mag;
                            break;
                        case DirJump:
                            mRunDirection.y += mag;
                            break;
                        case DirCrouch:
                            mRunDirection.y -= mag;
                            break;
                    }
                }

                void InputHandler::sendMoveMessage(const addutil::Vector3& to)
                {
                    if(mControlledPlayer > 0)
                        network->sendMessage(messages::MovePlayerControlMessage(mControlledPlayer, to));
                }

                void InputHandler::sendMoveMessage(Direction d, float mag)
                {
                    addutil::Vector3 v(0.0f, 0.0f, 0.0f);
                    switch(d)
                    {
                        case X_UP: v = addutil::Vector3(mag, 0.0f, 0.0f); break;
                        case Y_UP: v = addutil::Vector3(0.0f, mag, 0.0f); break;
                        case Z_UP: v = addutil::Vector3(0.0f, 0.0f, mag); break;
                        case X_DOWN: v = addutil::Vector3(-mag, 0.0f, 0.0f); break;
                        case Y_DOWN: v = addutil::Vector3(0.0f, -mag, 0.0f); break;
                        case Z_DOWN: v = addutil::Vector3(0.0f, 0.0f, -mag); break;
                    }
                    sendMoveMessage(v);
                }

                void InputHandler::switchToCameraMode(CameraMode m)
                {
                    mCamera->getParentSceneNode()->detachObject(mCamera);

                    int i;
                    switch(m)
                    {
                        case CamSky: 
                            i = 0; break;
                        case CamLengthFar: 
                            i = 1; break;
                        case CamTV: 
                        default: 
                            i = 2; break;
                    }
                    mCamNodes[i]->attachObject(mCamera);
                    mCamNode = mCamNodes[i];
                    mCamMode = m;

                    mCamera->setAutoTracking(false);
                    mCamera->setOrientation(Ogre::Quaternion::IDENTITY);
                    if(m == CamTV)
                    {
                        try
                        {
                            Ogre::SceneNode* ballnode = mSceneMgr->getSceneNode("Node-2");
                            mCamera->setAutoTracking(true, ballnode);
                        }
                        catch(...)
                        {
                            std::cerr << "InputHandler::switchToCameraMode(): ball node not found!\n";
                        }
                    }
                }
            }
        }
    }
}
