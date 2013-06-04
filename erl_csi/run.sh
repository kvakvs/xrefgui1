#!/bin/sh

mkdir -p ebin
erl -make && \
erl -pz ebin -s xrefgui_build start $1
