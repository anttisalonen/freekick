#!/bin/sh
cat DB/Countries/English.xml | sed -e 's/        <jersey>/        <jersey type="0">/g' -e '/ <type value="0"\/>/d' > DB/Countries/English2.xml
