#!/bin/bash

sudo apt install git openjdk-17-jdk make cmake gcc g++ libssl-dev zlib1g zlib1g-dev pkg-config build-essential curl libffi-dev libgmp-dev libgmp10 libncurses-dev libncurses5 libtinfo5
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
ghcup install ghc 8.10.7
ghcup set ghc 8.10.7
cabal update
git clone https://github.com/simplex-chat/simplex-chat -b master simplex
cd simplex
echo "ignore-project: False"    >> cabal.project.local
echo "package direct-sqlcipher" >> cabal.project.local
echo "    flags: +openssl"      >> cabal.project.local
scripts/desktop/build-lib-linux.sh

cd apps/multiplatform
sed -i 's|":android", ||' settings.gradle.kts

./gradlew packageDeb
#sudo dpkg -i ./release/main/deb/simpl*.deb
#sudo ln -s /opt/simplex/bin/simplex /usr/bin/simplex
