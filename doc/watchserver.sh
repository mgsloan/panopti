#!/bin/sh
# Example script to watch an activehs server
#
# Run this script regularly.
# For example in FreeBSD you can put a line into the crontab file:
#    env EDITOR=ee crontab -e
#    /1 * * * * /path-to-script/watchserver.sh


export DOCUMENTROOT=********;  # fill this in
export PATH=********;   # fill this in
export LANG="en_US.UTF-8";

# Date
D=`date +"%d/%b/%Y:%H:%M:%S"`;
# Request for the server
E="lang=en&c=eval&f=Syntax.hs&x=id \"$D\"&y=";
# Send the request
F=`curl --silent --show-error --max-time 15 --connect-timeout 10 --retry 3 --retry-delay 1 --data "$E" http://localhost:8000/ 2>err.txt`;
# Correct answer (each time different to by-pass the cache mechanism in activehs)
G="<code class=\"result\">&quot;$D&quot;</code><code> :: </code><code class=\"type\">[Char]</code>";

restartserver () {
  if pgrep activehs >/dev/null; then killall -9 activehs; fi;
  sleep 0.1;
  cd $DOCUMENTROOT; activehs --static +RTS -N -I2 2>/dev/null &
}

if [ "$F" = "$G" ];
then
  # Memory usage by the server
  M=`ps -A -o %mem= -o command= | grep -o -e '\([0-9]\+\)[.,][0-9]\+[ ]\+activehs' | sed -e 's/,//' -e 's/\.//'`;
  N=${M%activehs};
  # Limit memory usage
  if [ "$N" -gt "300" ];
  then 
    echo RESTART `date` CAUSE: Mem $N >> $DOCUMENTROOT/log/startserver.log;
    restartserver;
  else true;
  fi;
else 
  echo RESTART `date` CAUSE: Answer $F `cat err.txt` >> $DOCUMENTROOT/log/startserver.log;
  restartserver;
fi;


