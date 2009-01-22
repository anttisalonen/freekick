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

#include "Graphics.h"

namespace freekick
{
    namespace match
    {
        namespace client
        {
            namespace cl_ogre
            {
/**
 * @param  conf
 * @param  stat
 * @param  inp
 */
                Graphics::Graphics (Configuration* conf, MatchStatus* stat, Input* inp) 
                    : configuration(conf), status(stat), input(inp), updater(new GraphicsUpdater(stat))
                {
                }

                Graphics::~Graphics()
                {
                    delete updater;
                    delete mSystem;
/*
                    delete mRenderer;
                    delete mRoot;
*/
                }

                bool Graphics::run ()
                {
                    std::cout << "Graphics running" << std::endl;
                    createRoot();
                    defineResources();
                    setupRenderSystem();
                    createRenderWindow();
                    initializeResourceGroups();
                    setupScene();
                    setupInputSystem();
                    setupCEGUI();
                    startRenderLoop();
                    return true;
                }

/**
 * @return bool
 */
                bool Graphics::draw ( ) 
                {
                    return true;
                }

                void Graphics::createRoot()
                {
                    mRoot = new Ogre::Root();
                }

                void Graphics::defineResources()
                {
                    std::string secName, typeName, archName;
                    Ogre::ConfigFile cf;
                    cf.load("resources.cfg");

                    Ogre::ConfigFile::SectionIterator seci = cf.getSectionIterator();
                    while (seci.hasMoreElements())
                    {
                        secName = seci.peekNextKey();
                        Ogre::ConfigFile::SettingsMultiMap *settings = seci.getNext();
                        Ogre::ConfigFile::SettingsMultiMap::iterator i;
                        for (i = settings->begin(); i != settings->end(); ++i)
                        {
                            typeName = i->first;
                            archName = i->second;
                            Ogre::ResourceGroupManager::getSingleton().addResourceLocation(archName, typeName, secName);
                        }
                    }
                }

                void Graphics::setupRenderSystem()
                {
                    if (!mRoot->restoreConfig() && !mRoot->showConfigDialog())
                        throw Ogre::Exception(52, "User canceled the config dialog!", "Application::setupRenderSystem()");
                }

                void Graphics::createRenderWindow()
                {
                    mRoot->initialise(true, "Freekick OGRE client");
                }

                void Graphics::initializeResourceGroups()
                {
                    Ogre::TextureManager::getSingleton().setDefaultNumMipmaps(5);
                    Ogre::ResourceGroupManager::getSingleton().initialiseAllResourceGroups();
                }

                void Graphics::setupScene()
                {
                    using namespace Ogre;
                    SceneManager *mgr = mRoot->createSceneManager(ST_GENERIC, "Default SceneManager");
                    Camera *cam = mgr->createCamera("Camera");
                    Viewport *vp = mRoot->getAutoCreatedWindow()->addViewport(cam);

                    vp->setBackgroundColour(ColourValue(0.675, 0.84, 0.9));
                    cam->setNearClipDistance(0.5);
                    cam->setAspectRatio(Real(vp->getActualWidth()) / Real(vp->getActualHeight()));

                    mgr->setAmbientLight(ColourValue( 0.2, 0.2, 0.2 ) );
                    mgr->setShadowTechnique( SHADOWTYPE_STENCIL_ADDITIVE);

                    Light* light;
                    light = mgr->createLight("Light1");
                    light->setType(Light::LT_SPOTLIGHT);
                    light->setDiffuseColour(0.9, 0.9, 0.9);
                    light->setSpecularColour(0.8, 0.8, 0.8);
                    light->setDirection(1, -1, 1);
                    light->setPosition(Ogre::Vector3(0, 15, 0));
                    light->setSpotlightRange(Degree(0), Degree(80));

                    light = mgr->createLight("Light2");
                    light->setType(Light::LT_SPOTLIGHT);
                    light->setDiffuseColour(0.9, 0.9, 0.9);
                    light->setSpecularColour(0.8, 0.8, 0.8);
                    light->setDirection(-1, -1, 1);
                    light->setPosition(Ogre::Vector3(70, 15, 0));
                    light->setSpotlightRange(Degree(0), Degree(80));

                    light = mgr->createLight("Light3");
                    light->setType(Light::LT_SPOTLIGHT);
                    light->setDiffuseColour(0.9, 0.9, 0.9);
                    light->setSpecularColour(0.8, 0.8, 0.8);
                    light->setDirection(1, -1, -1);
                    light->setPosition(Ogre::Vector3(0, 15, 100));
                    light->setSpotlightRange(Degree(0), Degree(80));

                    light = mgr->createLight("Light4");
                    light->setType(Light::LT_SPOTLIGHT);
                    light->setDiffuseColour(0.9, 0.9, 0.9);
                    light->setSpecularColour(0.8, 0.8, 0.8);
                    light->setDirection(-1, -1, -1);
                    light->setPosition(Ogre::Vector3(70, 15, 100));
                    light->setSpotlightRange(Degree(0), Degree(80));

                    // Robot
                    //Entity *ent1 = mgr->createEntity ("robot", "robot.mesh");
                    //SceneNode *node1 = mgr->getRootSceneNode()->createChildSceneNode("RobotNode");
                    //node1->attachObject(ent1);

                    // Plane
                    Plane plane (Ogre::Vector3::UNIT_Y, 0);
                    MeshManager::getSingleton().createPlane(
                        "ground", 
                        ResourceGroupManager::DEFAULT_RESOURCE_GROUP_NAME, 
                        plane, 
                        70, 100, 20, 20, 
                        true, 1, 5, 5, Ogre::Vector3::UNIT_Z);
                    Ogre::Entity *plent = mgr->createEntity("groundEntity", "ground");
                    Ogre::SceneNode* plnode = mgr->getRootSceneNode()->createChildSceneNode();
                    plnode->attachObject(plent);
                    plnode->setPosition(Ogre::Vector3(35.0f, 0.0f, 50.0f));
                    plent->setMaterialName("Examples/GrassFloor");
                    plent->setCastShadows(false);

                    Ogre::Entity *ent1 = mgr->createEntity ("goal1", "Goal.mesh");
                    Ogre::SceneNode *node1 = mgr->getRootSceneNode()->createChildSceneNode("goal1Node");
                    node1->setPosition(Ogre::Vector3(35.0f, 1.0f, -4.2f));
                    node1->setScale(3.66f, 2.44f, 1.0f);
                    node1->pitch(Ogre::Radian(Math::HALF_PI));
                    node1->attachObject(ent1);

                    ent1 = mgr->createEntity ("goal2", "Goal.mesh");
                    node1 = mgr->getRootSceneNode()->createChildSceneNode("goal2Node");
                    node1->setPosition(Ogre::Vector3(35.0f, 1.0f, 104.5f));
                    node1->setScale(3.66f, 2.44f, 1.0f);
                    node1->pitch(-Ogre::Radian(Math::HALF_PI));
                    node1->yaw(Ogre::Radian(Math::PI));
                    node1->attachObject(ent1);

                    updater->setSceneManager(mgr);
                    mRoot->addFrameListener(updater);
                }

                void Graphics::setupInputSystem()
                {
                    size_t windowHnd = 0;
                    std::ostringstream windowHndStr;
                    Ogre::RenderWindow *win = mRoot->getAutoCreatedWindow();

                    win->getCustomAttribute("WINDOW", &windowHnd);
                    windowHndStr << windowHnd;

                    unsigned int width, height, depth;
                    int top, left;
                    win->getMetrics(width, height, depth, left, top);

                    Ogre::SceneManager *mgr = mRoot->getSceneManager("Default SceneManager");
                    Ogre::Camera* cam = mgr->getCamera("Camera");

                    try
                    {
                        input->setupInputSystem(windowHndStr.str(), width, height, mgr, cam);
                        mRoot->addFrameListener(input->getFrameListener());
                    }
                    catch (const OIS::Exception &e)
                    {
                        throw Ogre::Exception(42, e.eText, "Application::setupInputSystem");
                    }
                }

                void Graphics::setupCEGUI()
                {
                    Ogre::SceneManager *mgr = mRoot->getSceneManager("Default SceneManager");
                    Ogre::RenderWindow *win = mRoot->getAutoCreatedWindow();

                    // CEGUI setup
                    mRenderer = new CEGUI::OgreCEGUIRenderer(win, Ogre::RENDER_QUEUE_OVERLAY, false, 3000, mgr);
                    mSystem = new CEGUI::System(mRenderer);

                    // Other CEGUI setup here.
                }

                void Graphics::startRenderLoop()
                {
                    float fps = 0.0f;
                    if(!fps)
                    {
                        mRoot->startRendering();
                        return;
                    }

                    float frametime = 1.0f / fps;
                    unsigned long sleep_time = frametime * 1000000;
                    long time_left = 0;

                    using namespace boost::posix_time;
                    bool cont = true;
                    while (cont)
                    {
                        ptime before_time(microsec_clock::local_time());
                        cont = mRoot->renderOneFrame();
                        ptime after_time(microsec_clock::local_time());

                        time_period diff_time(before_time, after_time);
                        time_duration diff_dur = diff_time.length();
                        long us_diff = diff_dur.total_microseconds();
                        time_left = sleep_time - us_diff;
                        if(time_left > 0)
                            boost::this_thread::sleep(microseconds(time_left));
                        else
                        {
                            std::cerr << "Graphics::startRendering: overrun by " << -time_left << " microseconds; fps: " << fps << std::endl;
                        }
                    }
                }
            }
        }
    }
}
