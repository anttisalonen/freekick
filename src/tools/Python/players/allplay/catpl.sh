#!/bin/sh
cat $1 | sed -e '/<Players>/d' -e '/<\/Players>/d'  -e '/<Players\/>/d' > $2
