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
                InputHandler::InputHandler (const boost::shared_ptr<InputConfiguration>& inputconf, 
                                            MatchStatus* ms,
                                            const std::string& windowhnd, 
                                            Ogre::RenderWindow* win,
                                            Network* netw,
                                            Ogre::SceneManager* mgr,
                                            Ogre::Camera* cam)
                    : inputconfiguration(inputconf),
                      mMatchStatus(ms),
                      network(netw),
                      mControlledPlayer(110),
                      mSceneMgr(mgr),
                      mCamera(cam),
                      mRenderWindow(win)
                {
                    mRotate = 0.13;
                    mMove = 250;
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
                    // TODO: read pitch dimensions from matchstatus
                    mCamNodes[0] = mSceneMgr->getRootSceneNode()->createChildSceneNode("CamNode0");
                    mCamNodes[0]->setPosition (Ogre::Vector3(35.0f, 100.0f, 50.0f));
                    mCamNodes[0]->lookAt (Ogre::Vector3(35.0f, 0, 50.0f), Ogre::Node::TS_WORLD);
                    mCamNodes[0]->roll(Ogre::Radian(-Ogre::Math::HALF_PI));

                    mCamNodes[1] = mSceneMgr->getRootSceneNode()->createChildSceneNode("CamNode1");
                    mCamNodes[1]->setPosition (Ogre::Vector3(35.0f, 75.0f, 120.0f));
                    mCamNodes[1]->lookAt (Ogre::Vector3(35.0f, 0, 65.0f), Ogre::Node::TS_WORLD);

                    mCamNodes[1]->attachObject(mCamera);
                    mCamNode = mCamNodes[1];
                    mCamMode = CamLengthFar;

                    mRaySceneQuery = mSceneMgr->createRayQuery(Ogre::Ray());
                    // mRaySceneQuery->setWorldFragmentType(Ogre::SceneQuery::WFT_SINGLE_INTERSECTION);

                    network->sendMessage(messages::PlayerControlRequestMessage(mControlledPlayer));
                }

                InputHandler::~InputHandler()
                {
                    mSceneMgr->destroyQuery(mRaySceneQuery);

                    if(mKeyboard) mInputManager->destroyInputObject(mKeyboard);
                    if(mMouse) mInputManager->destroyInputObject(mMouse);
                    OIS::InputManager::destroyInputSystem(mInputManager);
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

                bool InputHandler::mousePressed (const OIS::MouseEvent& e, OIS::MouseButtonID id )
                {
                    // CEGUI::Point mousePos = CEGUI::MouseCursor::getSingleton().getPosition();
                    Ogre::Ray mouseRay = mCamera->getCameraToViewportRay(
                        e.state.X.abs / float(e.state.width), 
                        e.state.Y.abs / float(e.state.height));
                    mRaySceneQuery->setRay(mouseRay);
                    mRaySceneQuery->setSortByDistance(false);
                    // std::cerr << "X: " << e.state.X.abs << "; Y: " << e.state.Y.abs << "; width: " << e.state.width << "; height: " << e.state.height << std::endl;

                    using namespace Ogre;
                    RaySceneQueryResult& result = mRaySceneQuery->execute();
                    RaySceneQueryResult::iterator itr = result.begin();

                    for(itr = result.begin(); itr != result.end(); itr++)
                    {
                        if(itr->distance == 0.0f)
                            continue;
                        Ogre::Vector3 clickpos = mouseRay.getPoint(itr->distance);
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
                        break;
                    }

                    return true;
                }

                bool InputHandler::mouseReleased (const OIS::MouseEvent& e, OIS::MouseButtonID id )
                {
                    return true;
                }

                bool InputHandler::mouseMoved (const OIS::MouseEvent& e )
                {
                    // CEGUI::System::getSingleton().injectMouseMove(e.state.X.rel, e.state.Y.rel);
/*
                    if (e.state.buttonDown(OIS::MB_Right))
                    {
                        if(mCamNode)
                        {
                            mCamNode->yaw(Ogre::Degree(-mRotate * e.state.X.rel), Ogre::Node::TS_WORLD);
                            mCamNode->pitch(Ogre::Degree(-mRotate * e.state.Y.rel), Ogre::Node::TS_LOCAL);
                        }
                    }
*/
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
                            const Ogre::Vector3& vec = mCamNode->getPosition();
                            std::cerr << "Camera position: " << vec.x << "; " << vec.y << "; " << vec.z << std::endl;
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
                            network->sendMessage(messages::PlayerControlRequestMessage(mControlledPlayer));
                            break;

                        case OIS::KC_1:
                            switchToCameraMode(CamSky);
                            break;

                        case OIS::KC_2:
                            switchToCameraMode(CamLengthFar);
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
                        case OIS::KC_UP:
                            mDirection.z += mMove;
                            break;

                        case OIS::KC_DOWN:
                            mDirection.z -= mMove;
                            break;

                        case OIS::KC_LEFT:
                            mDirection.x += mMove;
                            break;

                        case OIS::KC_RIGHT:
                            mDirection.x -= mMove;
                            break;

                        case OIS::KC_PGDOWN:
                            mDirection.y += mMove;
                            break;

                        case OIS::KC_PGUP:
                            mDirection.y -= mMove;
                            break;

                        case OIS::KC_W:
                            setMoveVector(DirUp, -15.0f);
                            sendmove = true;
                            break;

                        case OIS::KC_S:
                            setMoveVector(DirDown, -15.0f);
                            sendmove = true;
                            break;

                        case OIS::KC_A:
                            setMoveVector(DirLeft, -15.0f);
                            sendmove = true;
                            break;

                        case OIS::KC_D:
                            setMoveVector(DirRight, -15.0f);
                            sendmove = true;
                            break;

                        case OIS::KC_Q:
                            setMoveVector(DirUp, -15.0f);
                            sendmove = true;
                            break;

                        case OIS::KC_E:
                            setMoveVector(DirCrouch, -15.0f);
                            sendmove = true;
                            break;

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
                            if(mCamMode == CamSky)
                                mRunDirection.x += mag;
                            else if (mCamMode == CamLengthFar)
                                mRunDirection.z -= mag;
                            break;
                        case DirDown:
                            if(mCamMode == CamSky)
                                mRunDirection.x -= mag;
                            else if (mCamMode == CamLengthFar)
                                mRunDirection.z += mag;
                            break;
                        case DirLeft:
                            if(mCamMode == CamSky)
                                mRunDirection.z -= mag;
                            else if (mCamMode == CamLengthFar)
                                mRunDirection.x -= mag;
                            break;
                        case DirRight:
                            if(mCamMode == CamSky)
                                mRunDirection.z += mag;
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
                    int i = (m == CamSky) ? 0 : 1;
                    mCamNodes[i]->attachObject(mCamera);
                    mCamNode = mCamNodes[i];
                    mCamMode = m;
                }
            }
        }
    }
}
