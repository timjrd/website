#!/bin/bash

modif() { stat Builds/SERVER --printf '%Y'; }

function handle
{
    rmdir $HOME/.run-sh-lock
    exit
}

function main
{
    trap handle SIGTERM SIGINT SIGQUIT
    mkdir -p .data-dir
    ln -srT static .data-dir/static

    last=0
    while true
    do
	if [ $last -ne $(modif) ]
	then
	    last=$(modif)
	    killall SERVER
	    sleep 1
	    killall -9 SERVER
	    sleep 0.1
	    Builds/SERVER 127.0.0.1 8000 .data-dir &
	fi
	sleep 3
    done
}

trap handle SIGTERM SIGINT SIGQUIT
if mkdir $HOME/.run-sh-lock 1> /dev/null 2> /dev/null
then
    main 1> /dev/null 2> /dev/null &
else
    echo 'Already running.'
    exit 1
fi






