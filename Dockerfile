FROM alpine:latest AS build-stage

# Alpine Linux doesn't provide libtinfow.so.6 library, so we need to symlink it.
# Add essential packages for ghc/cabal to work.
RUN apk add --no-cache \
    curl \
    git \
    xz \
    grep \
    ghc-dev \
    gmp-dev \
    zlib-static \
    zlib-dev \
    openssl-libs-static \
    openssl-dev \
    alpine-sdk &&\
    ln -s /usr/lib/libncursesw.so.6 /usr/lib/libtinfow.so.6

# Specify bootstrap Haskell versions
ENV BOOTSTRAP_HASKELL_GHC_VERSION=9.6.3
ENV BOOTSTRAP_HASKELL_CABAL_VERSION=3.10.1.0

# Install ghcup
RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | BOOTSTRAP_HASKELL_NONINTERACTIVE=1 sh

# Adjust PATH
ENV PATH="/root/.cabal/bin:/root/.ghcup/bin:$PATH"

# Set both as default
RUN ghcup set ghc "${BOOTSTRAP_HASKELL_GHC_VERSION}" && \
    ghcup set cabal "${BOOTSTRAP_HASKELL_CABAL_VERSION}"

# Install hpack
RUN cabal install --install-method=copy --installdir=/usr/local/bin hpack-0.36.0

# Copy project in PWD to dontainer
COPY . /project
WORKDIR /project

# Adjust build
RUN cp ./scripts/cabal.project.local.linux ./cabal.project.local

# Add optimization flags and build statically  
RUN sed -i '/- -Wunused-type-patterns/a\  - -O2\n\  - -split-sections\n\  - -with-rtsopts=-N\n\  - -static\n\cc-options: -static\n\ld-options: -static -pthread' package.yaml

# Reconfigure cabal project
RUN hpack

# Compile simplex-chat
RUN cabal update
RUN cabal build -j exe:simplex-chat

# Strip the binary from debug symbols to reduce size
RUN bin=$(find /project/dist-newstyle -name "simplex-chat" -type f -executable) && \
    mv "$bin" ./ && \
    strip ./simplex-chat

FROM scratch AS export-stage
COPY --from=build-stage /project/simplex-chat /
