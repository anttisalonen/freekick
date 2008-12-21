#!/bin/sh
cd bin
./client_ogre $1 $2 $3
# quick workaround for this one: http://www.wreckedgames.com/forum/index.php/topic,1029.0.html
xset r on
cd ..

