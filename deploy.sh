#!/bin/bash

SYNC='rsync -cz --progress '

function binaries
{
    echo ':: compiling...'
    make release

    echo -e '\n:: stopping the server...'
    rhc app stop -a me

    echo -e '\n:: uploading binaries...'
    $SYNC Builds/bin/* '5439726e5973ca6f4e000012@me-timjrd.rhcloud.com:~/app-root/repo/'

    echo -e '\n:: starting the server...'
    rhc app start -a me
}

function static
{
    echo ':: compiling css...'
    make css

    echo -e '\n:: uploading static files...'
    $SYNC -r static/* Builds/static/* '5439726e5973ca6f4e000012@me-timjrd.rhcloud.com:~/app-root/data/static'
}

case $1 in

--static-only  ) static ;;
--binaries-only) binaries ;;
''             ) binaries ; static ;;

esac
