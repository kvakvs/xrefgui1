#!/bin/sh

mkdir ebin
erl -make && \
erl -pz ebin -s learnkred
