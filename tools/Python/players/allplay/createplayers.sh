#!/bin/sh

i=0
nextid=1
for directory in players/*
do
    if [ -d $directory ]
    then
        let "i += 1"
        python players.py $nextid Cultures.xml eng.clubs English_Test.xml Players$i.xml $directory/*.txt
        if [ $? -ne 0 ]
        then
            echo Last start ID used: $nextid
            echo i = $i
            break
        fi
        let "nextid += 2000"
    fi
done

exit 0

