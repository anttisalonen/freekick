def_env = Environment()
def_env['CPPFLAGS'] = '-Wall -Wno-deprecated '
def_env.Append(CPPPATH = ['src'])

prof = ARGUMENTS.get('prof', 0)
if int(prof):
    def_env.Append(CPPFLAGS = '-pg ')
    def_env.Append(LINKFLAGS = '-pg ')

debug = ARGUMENTS.get('debug', 0)
if int(debug):
    def_env.Append(CPPFLAGS = '-g ')

opt = ARGUMENTS.get('opt', 1)
if int(opt):
    def_env.Append(CPPFLAGS = '-O2 ')

# SWOS2FK

swos2fk_env = def_env.Clone()
swos2fk_env.Append(CPPPATH = ['./src/tools/swos2fk'])
swos2fk_env.ParseConfig("pkg-config libxml-2.0 --cflags --libs")

swos2fk_name = 'bin/swos2fk'
swos2fk_files = Glob('src/tools/swos2fk/*.cpp')
swos2fk = swos2fk_env.Program(swos2fk_name, swos2fk_files)

# Libaddutil

addutil_env = def_env.Clone()
addutil_env.Append(CPPPATH = ['src/addutil'])
# addutil_env['CPPFLAGS'] += '-fPIC '
addutil_name = 'lib/addutil'
addutil_obj = addutil_env.Object(Glob('src/addutil/*.cpp') + 
                                 Glob('src/addutil/network/*.cpp') +
                                 Glob('src/addutil/ai/*.cpp'))
addutil_env.Library(addutil_name, addutil_obj)

# Libsoccer

soccer_env = addutil_env.Clone()
soccer_env.Append(CPPPATH = ['src/freekick/soccer'])
soccer_env['LIBPATH'] = ['./lib']
soccer_env['LIBS'] = ['addutil']
soccer_name = 'lib/soccer'
soccer_obj = soccer_env.Object(Glob('src/freekick/soccer/*.cpp')) + addutil_obj
soccer_env.Library(soccer_name, soccer_obj)

# Libmatch

match_env = addutil_env.Clone()
match_env.Append(CPPPATH = ['./src/freekick/soccer'])
match_env.Append(CPPPATH = ['./src/freekick/match'])

match_env.ParseConfig("pkg-config bullet --cflags --libs")

match_name = 'lib/match'
match_obj = match_env.Object(Glob('src/freekick/match/*.cpp') + 
                             Glob('src/freekick/match/messages/*.cpp')) + soccer_obj
match_env.Library(match_name, match_obj)

# Libclient

client_env = def_env.Clone()
client_env.Append(CPPPATH = ['./src/freekick/soccer'])
client_env.Append(CPPPATH = ['./src/freekick/match'])
client_env.Append(CPPPATH = ['./src/freekick/match/client'])

client_name = 'lib/client'
client_obj = client_env.Object(Glob('src/freekick/match/client/*.cpp')) + match_obj
client_env.Library(client_name, client_obj)

# OGRE Client

ogreclient_env = def_env.Clone()
ogreclient_env.Append(CPPPATH = ['./src/freekick/soccer'])
ogreclient_env.Append(CPPPATH = ['./src/freekick/match'])
ogreclient_env.Append(CPPPATH = ['./src/freekick/match/client'])
ogreclient_env.Append(CPPPATH = ['./src/freekick/match/client/cl_ogre'])
ogreclient_env.Append(LIBPATH = ['./lib'])

ogreclient_env.ParseConfig("pkg-config OGRE --cflags --libs")
ogreclient_env.ParseConfig("pkg-config CEGUI-OGRE --cflags --libs")
ogreclient_env.ParseConfig("pkg-config OIS --cflags --libs")
ogreclient_env.ParseConfig("pkg-config CEGUI --cflags --libs")
ogreclient_env['LIBS'] = ['boost_thread', 
                          'boost_system', 
                          'boost_serialization',
                          'boost_regex',
                          'OgreMain', 
                          'CEGUIBase', 
                          'OIS', 
                          'CEGUIOgreRenderer', 
                          'client']

ogreclient_name = 'bin/client_ogre'
ogreclient_files = (Glob('src/freekick/match/client/cl_ogre/*.cpp'))
ogre_client = ogreclient_env.Program(ogreclient_name, ogreclient_files)

# AI Client

aiclient_env = def_env.Clone()
aiclient_env.Append(CPPPATH = ['./src/addutil'])
aiclient_env.Append(CPPPATH = ['./src/freekick/soccer'])
aiclient_env.Append(CPPPATH = ['./src/freekick/match'])
aiclient_env.Append(CPPPATH = ['./src/freekick/match/client'])
aiclient_env.Append(CPPPATH = ['./src/freekick/match/client/ai'])
aiclient_env.Append(LIBPATH = ['./lib'])
aiclient_env['LIBS'] = ['boost_thread', 
                        'boost_system', 
                        'boost_serialization',
                        'boost_regex',
                        'client']

aiclient_name = 'bin/aiclient'
aiclient_files = (Glob('src/freekick/match/client/ai/*.cpp') +
                  Glob('src/freekick/match/client/ai/tasks/*.cpp'))
ai_client = aiclient_env.Program(aiclient_name, aiclient_files)

# Server

fkserver_env = def_env.Clone()
fkserver_env.Append(CPPPATH = ['./src/addutil'])
fkserver_env.Append(CPPPATH = ['./src/freekick'])
fkserver_env.Append(CPPPATH = ['./src/freekick/soccer'])
fkserver_env.Append(CPPPATH = ['./src/freekick/match'])
fkserver_env.Append(CPPPATH = ['./src/freekick/match/server'])
fkserver_env.Append(LIBPATH = ['./lib'])

fkserver_env.ParseConfig("pkg-config bullet --cflags --libs")

fkserver_conf = Configure(fkserver_env)

bulletlibs = ['bulletdynamics',
              'bulletcollision',
              'bulletmath']

bulletfound = True
for lib in bulletlibs:
    if not fkserver_conf.CheckLib(lib):
        bulletfound = False

if not bulletfound:
    print "Bullet libs not found, exiting."
    Exit(1)

fkserver_env['LIBS'] = ['boost_thread', 
                        'boost_system', 
                        'boost_serialization',
                        'boost_regex',
                        'match',
                         bulletlibs]

fkserver_name = 'bin/fkserver'
fkserver_files = Glob('src/freekick/match/server/*.cpp')
fkserver = fkserver_env.Program(fkserver_name, fkserver_files)

# freekick_client_py-client

freekick_client_py_env = def_env.Clone()
freekick_client_py_env.Append(CPPPATH = ['./src/freekick/match/client/ncurses'])
freekick_client_py_env.Append(CPPPATH = ['./src/addutil'])
freekick_client_py_env.Append(CPPPATH = ['./src/freekick/soccer'])
freekick_client_py_env.Append(CPPPATH = ['./src/freekick/match'])
freekick_client_py_env.Append(CPPPATH = ['./src/freekick/match/client'])
freekick_client_py_env.Append(CPPPATH = ['/usr/include/python2.5'])
freekick_client_py_env.Append(LIBPATH = ['./lib'])

freekick_client_py_env['LIBS'] = ['boost_thread', 
                                  'boost_system', 
                                  'boost_serialization',
                                  'boost_regex',
                                  'boost_python',
                                  'client']

freekick_client_py_name = 'lib/freekick_client_py'
freekick_client_py_files = Glob('src/freekick/match/client/python/*.cpp')
freekick_client_py = freekick_client_py_env.SharedLibrary(freekick_client_py_name, freekick_client_py_files)


Default(ai_client, ogre_client, fkserver)

print "Building", map(str, BUILD_TARGETS)
