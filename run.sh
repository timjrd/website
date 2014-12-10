#!/bin/bash

modif() { stat Builds/bin/SERVER --printf '%Y'; }


mkdir -p .data-dir
ln -srT static .data-dir/static
ln -sr  *.css .data-dir/static

last=0
while true
do
    if [ $last -ne $(modif) ]
    then
	echo -e "\n\n------------------------"
	echo -e     "** Restarting the server"
	last=$(modif)
	killall SERVER
	sleep 1
	killall -9 SERVER
	sleep 0.3
	echo
	Builds/bin/SERVER 127.0.0.1 8000 .data-dir &
    fi
    sleep 3
done
