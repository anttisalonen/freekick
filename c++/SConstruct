def_env = Environment()
def_env['CPPFLAGS'] = '-Wall -Wno-deprecated'


addutil_env = def_env.Clone()
addutil_env.Append(CPPPATH = ['./include/libaddutil'])
addutil_name = 'lib/addutil'
addutil_obj = addutil_env.Object(Glob('src/libaddutil/*.cpp'))
addutil_env.Library(addutil_name, addutil_obj)


matchcommon_env = addutil_env.Clone()
matchcommon_env.Append(CPPPATH = ['./include/matchcommon'])
matchcommon_name = 'lib/matchcommon'
matchcommon_obj = Glob('src/matchcommon/*.cpp') + addutil_obj
matchcommon_env.Library(matchcommon_name, matchcommon_obj)


client_env = def_env.Clone()
client_env.Append(CPPPATH = ['./include/libaddutil'])
client_env.Append(CPPPATH = ['./include/matchcommon'])
client_env.Append(CPPPATH = ['./include/client'])
client_env.Append(LIBPATH = ['./lib'])

client_env.ParseConfig("pkg-config OGRE --cflags --libs")
client_env.ParseConfig("pkg-config CEGUI-OGRE --cflags --libs")
client_env.ParseConfig("pkg-config OIS --cflags --libs")
client_env.ParseConfig("pkg-config CEGUI --cflags --libs")
client_env['LIBS'] = ['boost_thread', 
                      'boost_system', 
                      'OgreMain', 
                      'CEGUIBase', 
                      'OIS', 
                      'CEGUIOgreRenderer', 
                      'matchcommon']

client_name = 'bin/client_ogre'
client_files = Glob('src/client/*.cpp')
client_env.Program(client_name, client_files)
