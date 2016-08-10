#!/bin/bash

X=$1
WORKDIR=/var/www/linii

[ "$X" ] || { echo ERROR: first argument should be hostname where to deploy ; exit 1 ; }

ssh "$X" " mkdir -p $WORKDIR/config ; mkdir -p $WORKDIR/static/css ; mkdir -p $WORKDIR/static/fonts ;  "

rsync -cav --delete static/css/ $X:$WORKDIR/static/css/
rsync -cav --delete static/fonts/ $X:$WORKDIR/static/fonts/

scp vcard2dict.py json2vcard.py "$X":/usr/local/lib/amkhlv/

ssh "$X" "chown -R www-data:www-data $WORKDIR ; chmod a+x /usr/local/lib/amkhlv/vcard2dict.py ;  chmod a+x /usr/local/lib/amkhlv/json2vcard.py "

find .stack-work/dist/ -type f -path '*/build/linii/*' -name linii -exec scp {} $X:/usr/local/bin/ \;





echo "DO THE FOLLOWING:   scp linii.sqlite3 $X:$WORKDIR/
and chown it to www-data:www-data"


