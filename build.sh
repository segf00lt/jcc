#!/bin/sh

cd ./third_party/raylib && make RAYLIB_LIBTYPE=STATIC && cd ../..;

make

cd ./third_party/raylib && make RAYLIB_LIBTYPE=SHARED && cd ../..;
cp ./third_party/raylib/libraylib.a ./raylib;
cp -P ./third_party/raylib/libraylib.so* ./raylib;
