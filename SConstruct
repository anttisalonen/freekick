def_env = Environment()
def_env['CPPFLAGS'] = '-Wall -Wno-deprecated'

# Libaddutil

addutil_env = def_env.Clone()
addutil_env.Append(CPPPATH = ['./include/addutil', './include/addutil/network'])
addutil_name = 'lib/addutil'
addutil_obj = addutil_env.Object(Glob('src/addutil/*.cpp') + Glob('src/addutil/network/*.cpp'))
addutil_env.Library(addutil_name, addutil_obj)

# Libsoccer

soccer_env = addutil_env.Clone()
soccer_env.Append(CPPPATH = ['./include/'])
soccer_env.Append(CPPPATH = ['./include/freekick/soccer'])
soccer_env['LIBPATH'] = ['./lib']
soccer_env['LIBS'] = ['addutil']
soccer_name = 'lib/soccer'
soccer_obj = soccer_env.Object(Glob('src/freekick/soccer/*.cpp')) + addutil_obj
soccer_env.Library(soccer_name, soccer_obj)

# Libmatch

match_env = addutil_env.Clone()
match_env.Append(CPPPATH = ['./include/'])
match_env.Append(CPPPATH = ['./include/freekick'])
match_env.Append(CPPPATH = ['./include/freekick/soccer'])
match_env.Append(CPPPATH = ['./include/freekick/match'])
match_env.Append(CPPPATH = ['./include/freekick/match/messages'])
match_env.Append(CPPPATH = ['/usr/local/include/bullet'])
match_name = 'lib/match'
match_obj = match_env.Object(Glob('src/freekick/match/*.cpp') + 
                             Glob('src/freekick/match/messages/*.cpp')) + soccer_obj
match_env.Library(match_name, match_obj)

# Libclient

client_env = def_env.Clone()
client_env.Append(CPPPATH = ['./include'])
client_env.Append(CPPPATH = ['./include/addutil'])
client_env.Append(CPPPATH = ['./include/freekick/soccer'])
client_env.Append(CPPPATH = ['./include/freekick/match'])
client_env.Append(CPPPATH = ['./include/freekick/match/client'])

client_name = 'lib/client'
client_obj = client_env.Object(Glob('src/freekick/match/client/*.cpp')) + match_obj
client_env.Library(client_name, client_obj)

# OGRE Client

ogreclient_env = def_env.Clone()
ogreclient_env.Append(CPPPATH = ['./include'])
ogreclient_env.Append(CPPPATH = ['./include/addutil'])
ogreclient_env.Append(CPPPATH = ['./include/freekick/soccer'])
ogreclient_env.Append(CPPPATH = ['./include/freekick/match'])
ogreclient_env.Append(CPPPATH = ['./include/freekick/match/client'])
ogreclient_env.Append(CPPPATH = ['./include/freekick/match/client/cl_ogre'])
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
aiclient_env.Append(CPPPATH = ['./include'])
aiclient_env.Append(CPPPATH = ['./include/addutil'])
aiclient_env.Append(CPPPATH = ['./include/freekick/soccer'])
aiclient_env.Append(CPPPATH = ['./include/freekick/match'])
aiclient_env.Append(CPPPATH = ['./include/freekick/match/client'])
aiclient_env.Append(CPPPATH = ['./include/freekick/match/client/ai'])
aiclient_env.Append(LIBPATH = ['./lib'])
aiclient_env['LIBS'] = ['boost_thread', 
                        'boost_system', 
                        'boost_serialization',
                        'boost_regex',
                        'client']

aiclient_name = 'bin/aiclient'
aiclient_files = (Glob('src/freekick/match/client/ai/*.cpp'))
ai_client = aiclient_env.Program(aiclient_name, aiclient_files)

# Server

fkserver_env = def_env.Clone()
fkserver_env.Append(CPPPATH = ['./include'])
fkserver_env.Append(CPPPATH = ['./include/addutil'])
fkserver_env.Append(CPPPATH = ['./include/freekick/soccer'])
fkserver_env.Append(CPPPATH = ['./include/freekick/match'])
fkserver_env.Append(CPPPATH = ['./include/freekick/match/server'])
fkserver_env.Append(CPPPATH = ['/usr/local/include/bullet'])
fkserver_env.Append(LIBPATH = ['./lib'])

fkserver_conf = Configure(fkserver_env)
bullet273libs = ['LibBulletDynamics',
                 'LibBulletCollision',
                 'LibLinearMath']
bullet274libs = ['bulletdynamics',
                 'bulletcollision',
                 'bulletmath']

bullet274found = True
for lib in bullet274libs:
    if not fkserver_conf.CheckLib(lib):
        bullet274found = False

if bullet274found:
    bulletlib = bullet274libs
else:
    bullet273found = True
    for lib in bullet273libs:
        if not fkserver_conf.CheckLib(lib):
            bullet273found = False
    if bullet273found:
        bulletlib = bullet273libs
    else:
        print "Bullet libs not found, exiting."
        Exit(1)

fkserver_env['LIBS'] = ['boost_thread', 
                        'boost_system', 
                        'boost_serialization',
                        'boost_regex',
                        'match',
                         bulletlib]

fkserver_name = 'bin/fkserver'
fkserver_files = Glob('src/freekick/match/server/*.cpp')
fkserver = fkserver_env.Program(fkserver_name, fkserver_files)

Default(ai_client, ogre_client, fkserver)

print "Building", map(str, BUILD_TARGETS)
