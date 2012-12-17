#!/bin/sh

s=`./stub-sysctl hw.setperf`
old=`echo $s | sed 's/.*=//'`
if [ "100" = $old ] ; then
    new=0
else
    new=100
fi

sudo ./stub-sysctl -w hw.setperf $new > /dev/null

printf "cpu: %d -> %d\n" $old $new
speed=`./stub-sysctl hw.cpuspeed`
clock=`echo $speed | sed 's/.*=//'`
clock2=`echo "$clock / 1000" | bc -l`
printf "clock: %0.1f Ghz\n" $clock

exit 0
