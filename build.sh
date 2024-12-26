#!/bin/sh

OS_NAME=$(uname)
if [ "$OS_NAME" = "Linux" ]; then

if command -v apt-get >/dev/null 2>&1; then
    sudo apt-get install clang libasound2-dev libx11-dev libxrandr-dev libxi-dev libgl1-mesa-dev libglu1-mesa-dev libxcursor-dev libxinerama-dev libwayland-dev libxkbcommon-dev;
elif command -v apt >/dev/null 2>&1; then
    sudo apt install clang libasound2-dev libx11-dev libxrandr-dev libxi-dev libgl1-mesa-dev libglu1-mesa-dev libxcursor-dev libxinerama-dev libwayland-dev libxkbcommon-dev;
elif command -v pacman >/dev/null 2>&1; then
    sudo pacman -S alsa-lib mesa libx11 libxrandr libxi libxcursor libxinerama;
else
    echo "Unknown package manager";
    exit 1;
fi

fi

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

