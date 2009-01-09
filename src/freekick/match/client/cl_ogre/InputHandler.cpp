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
                InputHandler::InputHandler (boost::shared_ptr<InputConfiguration>& inputconf, 
                                            const std::string& windowhnd, 
                                            unsigned int width, 
                                            unsigned int height, 
                                            Ogre::SceneNode* c,
                                            Network* netw) 
                    : inputconfiguration(inputconf),
                      network(netw),
                      mControlledPlayer(110)
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
                    pl.insert(std::make_pair(std::string("w32_keyboard"), std::string("DISCL_NONEXCLUSIVE")));
#elif defined OIS_LINUX_PLATFORM
                    pl.insert(std::make_pair(std::string("x11_mouse_grab"), std::string("false")));
                    pl.insert(std::make_pair(std::string("x11_mouse_hide"), std::string("false")));
                    pl.insert(std::make_pair(std::string("x11_keyboard_grab"), std::string("false")));
                    pl.insert(std::make_pair(std::string("XAutoRepeatOn"), std::string("true")));
#endif

                    mInputManager = OIS::InputManager::createInputSystem(pl);

                    try
                    {
                        mKeyboard = static_cast<OIS::Keyboard*>(mInputManager->createInputObject(OIS::OISKeyboard, true));
                        mMouse = static_cast<OIS::Mouse*>(mInputManager->createInputObject(OIS::OISMouse, true));
                        //mJoy = static_cast<OIS::JoyStick*>(mInputManager->createInputObject(OIS::OISJoyStick, true));
                        const OIS::MouseState &ms = mMouse->getMouseState();
                        ms.width = width;
                        ms.height = height;
                        mMouse->setEventCallback(this);
                        mKeyboard->setEventCallback(this);
                    }
                    catch (const OIS::Exception& e)
                    {
                        std::cerr << "Error while initialising keyboard and mouse" << std::endl;
                        throw Ogre::Exception(42, e.eText, "InputHandler");
                    }

                    mCamNode = c;
                }

                InputHandler::~InputHandler()
                {
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
                    return true;
                }

                bool InputHandler::mouseReleased (const OIS::MouseEvent& e, OIS::MouseButtonID id )
                {
                    return true;
                }

                bool InputHandler::mouseMoved (const OIS::MouseEvent& e )
                {
                    if (e.state.buttonDown(OIS::MB_Right))
                    {
                        mCamNode->yaw(Ogre::Degree(-mRotate * e.state.X.rel), Ogre::Node::TS_WORLD);
                        mCamNode->pitch(Ogre::Degree(-mRotate * e.state.Y.rel), Ogre::Node::TS_LOCAL);
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

                        case OIS::KC_W:
                            sendmove = true;
                            mRunDirection.x = 15.0f;
                            break;

                        case OIS::KC_S:
                            sendmove = true;
                            mRunDirection.x = -15.0f;
                            break;

                        case OIS::KC_A:
                            sendmove = true;
                            mRunDirection.z = 15.0f;
                            break;

                        case OIS::KC_D:
                            sendmove = true;
                            mRunDirection.z = -15.0f;
                            break;

                        case OIS::KC_Q:
                            sendmove = true;
                            mRunDirection.y = 15.0f;
                            break;

                        case OIS::KC_E:
                            sendmove = true;
                            mRunDirection.y = -15.0f;
                            break;

                        case OIS::KC_X:
                            network->sendMessage(messages::PlayerControlRequestMessage(mControlledPlayer));
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
                            sendmove = true;
                            mRunDirection.x = 0.0f;
                            break;
                        case OIS::KC_S:
                            sendmove = true;
                            mRunDirection.x = 0.0f;
                            break;
                        case OIS::KC_A:
                            sendmove = true;
                            mRunDirection.z = 0.0f;
                            break;
                        case OIS::KC_D:
                            sendmove = true;
                            mRunDirection.z = 0.0f;
                            break;
                        case OIS::KC_E:
                            sendmove = true;
                            mRunDirection.y = 0.0f;
                            break;
                        case OIS::KC_Q:
                            sendmove = true;
                            mRunDirection.y = 0.0f;
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

                void InputHandler::setCamera(Ogre::SceneNode* c)
                {
                    mCamNode = c;
                }
            }
        }
    }
}
