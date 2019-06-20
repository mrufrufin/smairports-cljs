#!/bin/sh
/bin/fuser -k 8080/tcp
/bin/sleep 3
/usr/bin/node /home/pi/cljstuff/socketmusic-server/socketmusic.js /home/pi/cljstuff/smairports-cljs/public/ &
/bin/sleep 5
/usr/local/bin/pd -nogui -noadc -nodac -noverbose -noprefs /home/pi/cljstuff/smairports-cljs/smairports.pd

