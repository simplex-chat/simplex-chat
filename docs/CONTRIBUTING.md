# Contributing guide

## Compiling with SQLCipher encryption enabled

Add `cabal.project.local` to project root with the location of OpenSSL headers and libraries and flag setting encryption mode:

```
cp scripts/cabal.project.local.mac cabal.project.local
# or
# cp scripts/cabal.project.local.linux cabal.project.local
```

## OpenSSL on MacOS

MacOS comes with LibreSSL as default, OpenSSL must be installed to compile SimpleX from source.

OpenSSL can be installed with `brew install openssl@1.1`

You will have to add `/opt/homebrew/opt/openssl@1.1/bin` to your PATH in order to have things working properly
