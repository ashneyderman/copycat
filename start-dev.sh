#!/bin/sh

cd `dirname $0`

exec erl -pz $PWD/ebin \
    $PWD/deps/*/ebin \
    -s lager \
    -s copycat_app
