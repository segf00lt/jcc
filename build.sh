#!/bin/sh

if [ ! -e ./third_party/raylib/libraylib.a ]; then
    cd ./third_party/raylib && make && cd ../..;
fi

make

