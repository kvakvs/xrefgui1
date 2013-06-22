#!/bin/sh

if [ -z "$1" ] ; then
    echo "usage: run.sh config.conf; see csi.conf.example and use as template"
    exit 1
fi

mkdir -p ebin
erl -make && \
erl -pz ebin -s xrefgui_build start $1
