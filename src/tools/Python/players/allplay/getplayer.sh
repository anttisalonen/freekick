#!/bin/sh

for file in players_details*.html
do
  filetxt=`echo $file | tr -dc [:digit:]`.txt
  if [ -e $filetxt ]; then
    echo "Player $filetxt already prepared."
  else
    echo "Writing file $filetxt"
    echo $file | tr -dc [:digit:] > $filetxt
    echo >> $filetxt
    lynx --nolist --dump $file | head -n -17 | tac | head -n -26 | tac >> $filetxt
  fi
done

exit 0

