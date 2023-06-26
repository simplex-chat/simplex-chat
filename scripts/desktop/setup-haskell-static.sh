cd /
mkdir -p ghc/patches
cd ghc
git clone https://gitlab.haskell.org/ghc/ghc.git -b ghc-8.10.7-release --depth 1
cd ghc
ln -s ../patches patches
echo "@testing http://nl.alpinelinux.org/alpine/edge/testing" >> /etc/apk/repositories
apk add quilt quilt@testing
quilt new fpic-default.patch
# grep -R 'PIC' ./compiler to find a file with lines like `(OSOpenBSD, ArchX86_64)  -> [Opt_PIC] -- Due to PIE support in`
quilt edit compiler/main/DynFlags.hs
# add this line uncommented into list of platform:
# (OSLinux, ArchX86_64) -> [Opt_PIC]
quilt refresh
cp mk/build.mk.sample ../build.mk
cd ../
echo -e "GhcLibHcOpts += -fPIC\nGhcRtsHcOpts += -fPIC\nGhcRtsCcOpts += -fPIC\nBuildFlavour = quick" >> build.mk

ghcup compile ghc -j8 -v 8.10.7 -b 8.10.7 -p ${PWD}/patches -c ${PWD}/build.mk -o 8.10.7-fPIC -- --with-system-libffi