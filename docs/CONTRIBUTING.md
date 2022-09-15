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

OpenSSL can be installed with `brew install openssl`
