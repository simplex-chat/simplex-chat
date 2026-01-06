#!/usr/bin/env bash

set -e


ARCH="$(uname -m)"

function readlink() {
  echo "$(cd "$(dirname "$1")"; pwd -P)"
}

root_dir="$(dirname "$(dirname "$(readlink "$0")")")"
multiplatform_dir=$root_dir/apps/multiplatform
release_app_dir=$root_dir/apps/multiplatform/release/main/app

cd $multiplatform_dir
libcrypto_path=$(ldd common/src/commonMain/cpp/desktop/libs/*/libHSdirect-sqlcipher-*.so | grep libcrypto | cut -d'=' -f 2 | cut -d ' ' -f 2)
trap "rm common/src/commonMain/cpp/desktop/libs/*/`basename $libcrypto_path` 2> /dev/null || true" EXIT
cp $libcrypto_path common/src/commonMain/cpp/desktop/libs/*

./gradlew createDistributable
rm common/src/commonMain/cpp/desktop/libs/*/`basename $libcrypto_path`

rm -rf $release_app_dir/AppDir 2>/dev/null
mkdir -p $release_app_dir/AppDir/usr

cd $release_app_dir/AppDir
cp -r ../*imple*/{bin,lib} usr
cp usr/lib/simplex.png .

# For https://github.com/TheAssassin/AppImageLauncher to be able to show the icon
mkdir -p usr/share/{icons,metainfo,applications}
cp usr/lib/simplex.png usr/share/icons

ln -s usr/bin/*imple* AppRun
cp $multiplatform_dir/desktop/src/jvmMain/resources/distribute/*imple*.desktop chat.simplex.app.desktop
sed -i 's|Exec=.*|Exec=simplex|g' *imple*.desktop
sed -i 's|Icon=.*|Icon=simplex|g' *imple*.desktop
cp *imple*.desktop usr/share/applications/
cp $multiplatform_dir/desktop/src/jvmMain/resources/distribute/*.appdata.xml usr/share/metainfo

if [ ! -f ../appimagetool-${ARCH}.AppImage ]; then
    wget --secure-protocol=TLSv1_3 https://github.com/simplex-chat/appimagetool/releases/download/continuous/appimagetool-${ARCH}.AppImage -O ../appimagetool-${ARCH}.AppImage
    chmod +x ../appimagetool-${ARCH}.AppImage
fi
if [ ! -f ../runtime-${ARCH} ]; then
    wget --secure-protocol=TLSv1_3 https://github.com/simplex-chat/type2-runtime/releases/download/continuous/runtime-${ARCH} -O ../runtime-${ARCH}
    chmod +x ../runtime-${ARCH}
fi

# Determenistic build

export SOURCE_DATE_EPOCH=1704067200

# Delete redundant jar file and modify cfg
rm -f ./usr/lib/app/*skiko-awt-runtime-linux*
sed -i -e '/skiko-awt-runtime-linux/d' ./usr/lib/app/simplex.cfg

# Set all files to fixed time
find . -exec touch -d "@$SOURCE_DATE_EPOCH" {} +

../appimagetool-${ARCH}.AppImage --verbose --no-appstream --runtime-file ../runtime-${ARCH} .
mv *imple*.AppImage ../../

# Just a safeguard
strip-nondeterminism ../../*imple*.AppImage
