#!/bin/sh

OS_NAME=$(uname)

cd ./third_party/raylib && make RAYLIB_LIBTYPE=STATIC && cd ../..;

make

cd ./third_party/raylib && make RAYLIB_LIBTYPE=SHARED && cd ../..;
cp ./third_party/raylib/libraylib.a ./raylib;
if [ "$OS_NAME" = "Linux" ]; then
    mv ./third_party/raylib/libraylib.so* ./raylib;
elif [ "$OS_NAME" = "Darwin" ]; then
    mv ./third_party/raylib/libraylib.*dylib ./raylib;
else
    echo "Unkown OS: $OS_NAME";
fi
