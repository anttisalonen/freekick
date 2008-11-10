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


#ifndef INPUTHANDLER_H
#define INPUTHANDLER_H

#include <iostream>

#include <boost/shared_ptr.hpp>

#include <Ogre.h>

#include <OIS/OIS.h>

#include "InputConfiguration.h"

/**
  * class InputHandler
  */

class InputHandler : public Ogre::FrameListener, public OIS::MouseListener, public OIS::KeyListener
{
public:

    /**
     * @param  inputconf
     */
    InputHandler (InputConfiguration* inputconf, const std::string& windowhnd, unsigned int width, unsigned int height, Ogre::SceneNode* c);
    virtual ~InputHandler();

    bool frameStarted(const Ogre::FrameEvent& evt);
    bool mousePressed (const OIS::MouseEvent& e, OIS::MouseButtonID id );
    bool mouseReleased (const OIS::MouseEvent& e, OIS::MouseButtonID id );
    bool mouseMoved (const OIS::MouseEvent& e );
    bool keyPressed (const OIS::KeyEvent& e );
    bool keyReleased (const OIS::KeyEvent& e );
    void setCamera(Ogre::SceneNode* c);

private:

    // Private attributes
    //  

    int scrollState;
    InputConfiguration* inputconfiguration;
    OIS::MouseState mousePositionState;
    Ogre::SceneNode* camtarget;
    Ogre::SceneNode* mCamNode;

    Ogre::Real mRotate;          // The rotate constant
    Ogre::Real mMove;            // The movement constant

    bool mContinue;        // Whether to continue rendering or not
    Ogre::Vector3 mDirection;     // Value to move in the correct direction

    OIS::Keyboard *mKeyboard;
    OIS::Mouse *mMouse;
    OIS::InputManager *mInputManager;

};

#endif // INPUTHANDLER_H
