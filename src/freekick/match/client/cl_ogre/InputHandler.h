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


#ifndef INPUTHANDLER_H
#define INPUTHANDLER_H

#include <iostream>

#include <boost/shared_ptr.hpp>

#include <Ogre.h>

#include <OIS/OIS.h>

#include <CEGUI/CEGUISystem.h>
#include <CEGUI/CEGUISchemeManager.h>
#include <OgreCEGUIRenderer.h>

#include "addutil/Vector3.h"

#include "messages/InitialDataRequest.h"
#include "messages/MovePlayerControlMessage.h"
#include "messages/KickPlayerControlMessage.h"
#include "messages/PlayerControlRequestMessage.h"
#include "MatchPlayer.h"
#include "MatchStatus.h"
#include "Helpers.h"

#include "Network.h"
#include "InputConfiguration.h"

/**
 * class InputHandler
 */

namespace freekick
{
    namespace match
    {
        namespace client
        {
            namespace cl_ogre
            {
                enum Direction
                {
                    X_UP,
                    X_DOWN,
                    Y_UP,
                    Y_DOWN,
                    Z_UP,
                    Z_DOWN
                };

                enum SubjectiveDirection
                {
                    DirUp,
                    DirDown,
                    DirLeft,
                    DirRight,
                    DirJump,
                    DirCrouch
                };

                enum CameraMode
                {
                    CamSky,
                    CamLengthFar,
                    CamTV
                };

                class InputHandler : public Ogre::FrameListener, public OIS::MouseListener, public OIS::KeyListener, public Ogre::WindowEventListener
                {
                public:
                    InputHandler (int player_id,
                                  bool follow_mouse,
                                  const boost::shared_ptr<InputConfiguration>& inputconf,
                                  MatchStatus* ms,
                                  const std::string& windowhnd, 
                                  Ogre::RenderWindow* win,
                                  Network* netw,
                                  Ogre::SceneManager* mgr,
                                  Ogre::Camera* cam);
                    virtual ~InputHandler();

                    bool frameStarted(const Ogre::FrameEvent& evt);
                    bool mousePressed (const OIS::MouseEvent& e, OIS::MouseButtonID id );
                    bool mouseReleased (const OIS::MouseEvent& e, OIS::MouseButtonID id );
                    bool mouseMoved (const OIS::MouseEvent& e );
                    bool keyPressed (const OIS::KeyEvent& e );
                    bool keyReleased (const OIS::KeyEvent& e );
                    void windowResized(Ogre::RenderWindow* rw);

                private:
                    void init_camera_nodes();
                    void setMoveVector(SubjectiveDirection d, float mag);
                    void sendMoveMessage(const addutil::Vector3& to);
                    void sendMoveMessage(Direction d, float mag);
                    void switchToCameraMode(CameraMode m);
                    Ogre::Vector3 getPointedPitchPosition(const OIS::MouseEvent& e) const;

                    boost::shared_ptr<InputConfiguration> inputconfiguration;
                    MatchStatus* mMatchStatus;
                    // OIS::MouseState mousePositionState;
                    Ogre::SceneNode* camtarget;

                    Ogre::Real mRotate;          // The rotate constant
                    Ogre::Real mMove;            // The movement constant

                    bool mContinue;        // Whether to continue rendering or not
                    Ogre::Vector3 mDirection;     // Value to move in the correct direction
                    addutil::Vector3 mRunDirection;

                    OIS::Keyboard *mKeyboard;
                    OIS::Mouse *mMouse;
                    OIS::InputManager *mInputManager;

                    Network* network;

                    int mControlledPlayer;
                    Ogre::SceneManager* mSceneMgr;
                    Ogre::Camera* mCamera;
                    CameraMode mCamMode;
                    boost::array<Ogre::SceneNode*, 3> mCamNodes;
                    Ogre::SceneNode* mCamNode;

                    Ogre::RaySceneQuery* mRaySceneQuery;
                    CEGUI::Renderer *mGUIRenderer;
                    Ogre::RenderWindow* mRenderWindow;

                    bool mFollowMouse;
                };
            }
        }
    }
}

#endif // INPUTHANDLER_H
