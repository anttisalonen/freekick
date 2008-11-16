def_env = Environment()
def_env['CPPFLAGS'] = '-Wall -Wno-deprecated'


addutil_env = def_env.Clone()
addutil_env.Append(CPPPATH = ['./include/addutil'])
addutil_name = 'lib/addutil'
addutil_obj = addutil_env.Object(Glob('src/addutil/*.cpp'))
addutil_env.Library(addutil_name, addutil_obj)

soccer_env = addutil_env.Clone()
soccer_env.Append(CPPPATH = ['./include/'])
soccer_env.Append(CPPPATH = ['./include/freekick/soccer'])
soccer_env['LIBPATH'] = ['./lib']
soccer_env['LIBS'] = ['addutil']
soccer_name = 'lib/soccer'
soccer_obj = Glob('src/freekick/soccer/*.cpp') + addutil_obj
soccer_env.Library(soccer_name, soccer_obj)

match_env = addutil_env.Clone()
match_env.Append(CPPPATH = ['./include/'])
match_env.Append(CPPPATH = ['./include/freekick'])
match_env.Append(CPPPATH = ['./include/freekick/soccer'])
match_env.Append(CPPPATH = ['./include/freekick/match'])
match_env.Append(CPPPATH = ['./include/freekick/match/network'])
match_name = 'lib/match'
match_obj = Glob('src/freekick/match/*.cpp') + Glob('src/freekick/match/network/*.cpp') + soccer_obj
match_env.Library(match_name, match_obj)


ogreclient_env = def_env.Clone()
ogreclient_env.Append(CPPPATH = ['./include'])
ogreclient_env.Append(CPPPATH = ['./include/addutil'])
ogreclient_env.Append(CPPPATH = ['./include/freekick/soccer'])
ogreclient_env.Append(CPPPATH = ['./include/freekick/match'])
ogreclient_env.Append(CPPPATH = ['./include/freekick/match/network'])
ogreclient_env.Append(CPPPATH = ['./include/freekick/match/client'])
ogreclient_env.Append(CPPPATH = ['./include/freekick/match/client/cl_ogre'])
ogreclient_env.Append(LIBPATH = ['./lib'])

ogreclient_env.ParseConfig("pkg-config OGRE --cflags --libs")
ogreclient_env.ParseConfig("pkg-config CEGUI-OGRE --cflags --libs")
ogreclient_env.ParseConfig("pkg-config OIS --cflags --libs")
ogreclient_env.ParseConfig("pkg-config CEGUI --cflags --libs")
ogreclient_env['LIBS'] = ['boost_thread', 
                      'boost_system', 
                      'OgreMain', 
                      'CEGUIBase', 
                      'OIS', 
                      'CEGUIOgreRenderer', 
                      'match']

ogreclient_name = 'bin/client_ogre'
ogreclient_files = Glob('src/freekick/match/client/*.cpp') + Glob('src/freekick/match/client/cl_ogre/*.cpp')
ogre_client = ogreclient_env.Program(ogreclient_name, ogreclient_files)
