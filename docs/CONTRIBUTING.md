# Contributing guide

## Compiling with SQLCipher encryption enabled

Add `cabal.project.local` to project root with the location of OpenSSL headers and libraries and flag setting encryption mode:

```
ignore-project: False

package direct-sqlcipher
    extra-include-dirs: /opt/homebrew/opt/openssl@3/include
    extra-lib-dirs: /opt/homebrew/opt/openssl@3/lib
    flags: +openssl
```

## OpenSSL on macOS

macOS comes with LibreSSL as default thus modern OpenSSL packages must be installed to compile SimpleX from source.

OpenSSL can be installed using Homebrew package manager with `brew install openssl@3`

You will have to add `/opt/homebrew/opt/openssl@3/bin` to your PATH in order to have things working properly
