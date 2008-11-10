#!/bin/sh
python createclubvalue.py 21.txt 950 750 > eng1.txt
python createclubvalue.py 22.txt 750 600 > eng2.txt
python createclubvalue.py 23.txt 600 475 > eng3.txt
python createclubvalue.py 24.txt 475 350 > eng4.txt
python createclubvalue.py 29.txt 350 250 > eng5.txt
cat eng[12345].txt > eng.txt
