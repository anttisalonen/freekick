import os
num_cpu = int(os.environ.get('NUM_CPU', 1))
SetOption('num_jobs', num_cpu)
print "running with -j", GetOption('num_jobs')

def_env = Environment()
def_env['CPPFLAGS'] = '-Wall -Wno-deprecated '
def_env.Append(CPPPATH = ['src'])
def_env.ParseConfig("pkg-config libxml-2.0 --cflags --libs")

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


swos2fk_name = 'bin/swos2fk'
addutil_name = 'lib/addutil'
addutil_test1_name = 'bin/tests/vector3_test'
soccer_name = 'lib/soccer'
match_name = 'lib/match'
client_name = 'lib/client'
ogreclient_name = 'bin/client_ogre'
aiclient_name = 'bin/aiclient'
fkserver_name = 'bin/fkserver'
freekick_client_py_name = 'lib/freekick_client_py'
ncursesclient_name = 'bin/ncclient'

if len(BUILD_TARGETS) == 0:
   BUILD_TARGETS = [ogreclient_name, aiclient_name, fkserver_name]

print "Building", map(str, BUILD_TARGETS)

clients = [client_name, ogreclient_name, aiclient_name, freekick_client_py_name, ncursesclient_name]
servers = [fkserver_name]

build_libs = False
for t in clients + servers:
    if t in BUILD_TARGETS:
        build_libs = True
        break

# SWOS2FK

if swos2fk_name in BUILD_TARGETS:
    swos2fk_env = def_env.Clone()
    swos2fk_env.Append(CPPPATH = ['./src/tools/swos2fk'])
    swos2fk_env['LIBPATH'] = ['./lib']
    swos2fk_env['LIBS'] += ['addutil']
    swos2fk_files = Glob('src/tools/swos2fk/*.cpp')
    swos2fk = swos2fk_env.Program(swos2fk_name, swos2fk_files)

# Libaddutil

addutil_env = def_env.Clone()
addutil_env.Append(CPPPATH = ['src/addutil'])
# addutil_env['CPPFLAGS'] += '-fPIC '
addutil_obj = addutil_env.Object(Glob('src/addutil/*.cpp') + 
                                 Glob('src/addutil/network/*.cpp') +
                                 Glob('src/addutil/ai/*.cpp'))
addutil_env.Library(addutil_name, addutil_obj)


# Libaddutil tests

if addutil_test1_name in BUILD_TARGETS:
    addutil_test_env = Environment()
    addutil_test_env['CPPFLAGS'] = '-Wall -Wno-deprecated '
    addutil_test_env.Append(CPPPATH = ['src'])
    addutil_test_env.Program(addutil_test1_name, 'src/addutil/tests/Test_Vector3.cpp')
    addutil_test_env['LIBPATH'] = ['./lib']
    addutil_test_env['LIBS'] = ['addutil']
    addutil_test_env['LIBS'] += ['boost_unit_test_framework']

# Libraries

if build_libs:
    # Libsoccer
    soccer_env = addutil_env.Clone()
    soccer_env.Append(CPPPATH = ['src/freekick/soccer'])
    soccer_env['LIBPATH'] = ['./lib']
    soccer_env['LIBS'] += ['addutil']
    soccer_obj = soccer_env.Object(Glob('src/freekick/soccer/*.cpp')) + addutil_obj
    soccer_env.Library(soccer_name, soccer_obj)

    # Libmatch
    match_env = addutil_env.Clone()
    match_env.Append(CPPPATH = ['./src/freekick/soccer'])
    match_env.Append(CPPPATH = ['./src/freekick/match'])

    match_env.ParseConfig("pkg-config bullet --cflags --libs")

    match_obj = match_env.Object(Glob('src/freekick/match/*.cpp') + 
                                 Glob('src/freekick/match/messages/*.cpp')) + soccer_obj
    match_env.Library(match_name, match_obj)

    # Libclient
    client_env = def_env.Clone()
    client_env.Append(CPPPATH = ['./src/freekick/soccer'])
    client_env.Append(CPPPATH = ['./src/freekick/match'])
    client_env.Append(CPPPATH = ['./src/freekick/match/client'])

    client_obj = client_env.Object(Glob('src/freekick/match/client/*.cpp')) + match_obj
    client_env.Library(client_name, client_obj)


# OGRE Client

if ogreclient_name in BUILD_TARGETS:
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
    ogreclient_env['LIBS'] += ['boost_thread', 
                               'boost_system', 
                               'boost_serialization',
                               'boost_regex',
                               'client']

    ogreclient_files = (Glob('src/freekick/match/client/cl_ogre/*.cpp'))
    ogre_client = ogreclient_env.Program(ogreclient_name, ogreclient_files)


# NCurses Client

if ncursesclient_name in BUILD_TARGETS:
    ncursesclient_env = def_env.Clone()
    ncursesclient_env.Append(CPPPATH = ['./src/addutil'])
    ncursesclient_env.Append(CPPPATH = ['./src/freekick/soccer'])
    ncursesclient_env.Append(CPPPATH = ['./src/freekick/match'])
    ncursesclient_env.Append(CPPPATH = ['./src/freekick/match/client'])
    ncursesclient_env.Append(CPPPATH = ['./src/freekick/match/client/ncurses'])
    ncursesclient_env.Append(LIBPATH = ['./lib'])
    ncursesclient_env['LIBS'] += ['boost_thread',
                                  'boost_system',
                                  'boost_serialization',
                                  'boost_regex',
                                  'ncurses',
                                  'client']

    ncursesclient_files = (Glob('src/freekick/match/client/ncurses/*.cpp'))
    ncursesclient = ncursesclient_env.Program(ncursesclient_name, ncursesclient_files)


# AI Client

if aiclient_name in BUILD_TARGETS:
    aiclient_env = def_env.Clone()
    aiclient_env.Append(CPPPATH = ['./src/addutil'])
    aiclient_env.Append(CPPPATH = ['./src/freekick/soccer'])
    aiclient_env.Append(CPPPATH = ['./src/freekick/match'])
    aiclient_env.Append(CPPPATH = ['./src/freekick/match/client'])
    aiclient_env.Append(CPPPATH = ['./src/freekick/match/client/ai'])
    aiclient_env.Append(LIBPATH = ['./lib'])
    aiclient_env['LIBS'] += ['boost_thread', 
                             'boost_system', 
                             'boost_serialization',
                             'boost_regex',
                             'boost_program_options',
                             'client']

    aiclient_files = (Glob('src/freekick/match/client/ai/*.cpp'))
    ai_client = aiclient_env.Program(aiclient_name, aiclient_files)


bulletlibs = ['bulletdynamics',
              'bulletcollision',
              'bulletmath']

# Server

if fkserver_name in BUILD_TARGETS:
    fkserver_env = def_env.Clone()
    fkserver_env.Append(CPPPATH = ['./src/addutil'])
    fkserver_env.Append(CPPPATH = ['./src/freekick'])
    fkserver_env.Append(CPPPATH = ['./src/freekick/soccer'])
    fkserver_env.Append(CPPPATH = ['./src/freekick/match'])
    fkserver_env.Append(CPPPATH = ['./src/freekick/match/server'])
    fkserver_env.Append(LIBPATH = ['./lib'])

    fkserver_env.ParseConfig("pkg-config bullet --cflags --libs")

    fkserver_conf = Configure(fkserver_env)

    fkserver_env['LIBS'] += ['boost_thread', 
                             'boost_system', 
                             'boost_serialization',
                             'boost_regex',
                             'match',
                             bulletlibs]

    fkserver_files = Glob('src/freekick/match/server/*.cpp')
    fkserver = fkserver_env.Program(fkserver_name, fkserver_files)


# freekick_client_py-client

if freekick_client_py_name in BUILD_TARGETS:
    freekick_client_py_env = def_env.Clone()
    freekick_client_py_env.Append(CPPPATH = ['./src/freekick/match/client/ncurses'])
    freekick_client_py_env.Append(CPPPATH = ['./src/addutil'])
    freekick_client_py_env.Append(CPPPATH = ['./src/freekick/soccer'])
    freekick_client_py_env.Append(CPPPATH = ['./src/freekick/match'])
    freekick_client_py_env.Append(CPPPATH = ['./src/freekick/match/client'])
    freekick_client_py_env.Append(CPPPATH = ['/usr/include/python2.5'])
    freekick_client_py_env.Append(LIBPATH = ['./lib'])

    freekick_client_py_env['LIBS'] += ['boost_thread', 
                                       'boost_system', 
                                       'boost_serialization',
                                       'boost_regex',
                                       'boost_python',
                                       'client']

    freekick_client_py_files = Glob('src/freekick/match/client/python/*.cpp')
    freekick_client_py = freekick_client_py_env.SharedLibrary(freekick_client_py_name, freekick_client_py_files)

