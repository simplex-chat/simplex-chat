#!/bin/zsh
set -euo pipefail

SCRIPT_DIR=${0:A:h}
REPO_ROOT=${SCRIPT_DIR:h:h}
CORE_LIB_DIR=${SIMPLEX_CORE_LIB_DIR:-${REPO_ROOT}/apps/multiplatform/release/main/app/SimpleX.app/Contents/app/resources}
OUTPUT_DIR=${SIMPLEX_NATIVE_OUTPUT_DIR:-/private/tmp/simplex-native-build}
APP_DIR=${OUTPUT_DIR}/SimpleX.app

if [[ ! -f ${CORE_LIB_DIR}/libsimplex.dylib ]]; then
  print -u2 "SimpleX core libraries not found at ${CORE_LIB_DIR}"
  exit 1
fi

swift build --disable-sandbox --package-path ${SCRIPT_DIR} -c release --product SimpleXNative

if [[ -d ${APP_DIR} ]]; then
  BUILD_TIMESTAMP=$(date +%Y%m%d-%H%M%S)
  mv ${APP_DIR} ${OUTPUT_DIR}/SimpleX.previous.${BUILD_TIMESTAMP}.app
fi

mkdir -p ${APP_DIR}/Contents/MacOS ${APP_DIR}/Contents/Frameworks ${APP_DIR}/Contents/Resources
cp ${SCRIPT_DIR}/.build/release/SimpleXNative ${APP_DIR}/Contents/MacOS/SimpleX
cp ${CORE_LIB_DIR}/*.dylib ${APP_DIR}/Contents/Frameworks/
cp ${REPO_ROOT}/apps/multiplatform/desktop/src/jvmMain/resources/distribute/simplex.icns ${APP_DIR}/Contents/Resources/SimpleX.icns

plutil -create xml1 ${APP_DIR}/Contents/Info.plist
plutil -insert CFBundleDevelopmentRegion -string en ${APP_DIR}/Contents/Info.plist
plutil -insert CFBundleExecutable -string SimpleX ${APP_DIR}/Contents/Info.plist
plutil -insert CFBundleIconFile -string SimpleX ${APP_DIR}/Contents/Info.plist
plutil -insert CFBundleIdentifier -string chat.simplex.native ${APP_DIR}/Contents/Info.plist
plutil -insert CFBundleInfoDictionaryVersion -string 6.0 ${APP_DIR}/Contents/Info.plist
plutil -insert CFBundleName -string SimpleX ${APP_DIR}/Contents/Info.plist
plutil -insert CFBundlePackageType -string APPL ${APP_DIR}/Contents/Info.plist
plutil -insert CFBundleShortVersionString -string 7.0.0 ${APP_DIR}/Contents/Info.plist
plutil -insert CFBundleVersion -string 1 ${APP_DIR}/Contents/Info.plist
plutil -insert LSApplicationCategoryType -string public.app-category.social-networking ${APP_DIR}/Contents/Info.plist
plutil -insert LSMinimumSystemVersion -string 14.0 ${APP_DIR}/Contents/Info.plist
plutil -insert NSHighResolutionCapable -bool YES ${APP_DIR}/Contents/Info.plist
plutil -insert NSHumanReadableCopyright -string "Copyright © 2020-2026 SimpleX Chat" ${APP_DIR}/Contents/Info.plist
plutil -insert NSPrincipalClass -string NSApplication ${APP_DIR}/Contents/Info.plist
plutil -insert SimpleXKeychainPassphraseStorageEnabled -bool NO ${APP_DIR}/Contents/Info.plist

xattr -cr ${APP_DIR}
codesign --force --deep --sign - ${APP_DIR}
print ${APP_DIR}
