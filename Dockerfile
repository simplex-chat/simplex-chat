ARG TAG=22.04

FROM ubuntu:${TAG} AS build

### Build stage

# Install curl and git and simplex-chat dependencies
RUN apt-get update && apt-get install -y curl git build-essential libgmp3-dev zlib1g-dev llvm-12 llvm-12-dev libnuma-dev libssl-dev

# Specify bootstrap Haskell versions
ENV BOOTSTRAP_HASKELL_GHC_VERSION=9.6.4
ENV BOOTSTRAP_HASKELL_CABAL_VERSION=3.10.1.0

# Install ghcup
RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | BOOTSTRAP_HASKELL_NONINTERACTIVE=1 sh

# Adjust PATH
ENV PATH="/root/.cabal/bin:/root/.ghcup/bin:$PATH"

# Set both as default
RUN ghcup set ghc "${BOOTSTRAP_HASKELL_GHC_VERSION}" && \
    ghcup set cabal "${BOOTSTRAP_HASKELL_CABAL_VERSION}"

COPY . /project
WORKDIR /project

# Adjust build
RUN cp ./scripts/cabal.project.local.linux ./cabal.project.local

# Compile simplex-chat
RUN cabal update
RUN cabal build exe:simplex-chat

# Strip the binary from debug symbols to reduce size
RUN bin=$(find /project/dist-newstyle -name "simplex-chat" -type f -executable) && \
    mv "$bin" ./ && \
    strip ./simplex-chat

# Copy compiled app from build stage
FROM scratch AS export-stage
COPY --from=build /project/simplex-chat /
