#!/bin/sh
cat England.xml | grep -C 2 stadium | sed ':a; /[a-zA-Z ]">$/N; s/>\n//g; ta; s/ *<capacity value=/ capacity=/g' > stad.xml
