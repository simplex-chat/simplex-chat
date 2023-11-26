FROM ubuntu:focal AS build

# Install curl and simplex-chat-related dependencies
RUN apt-get update && apt-get install -y curl git build-essential libgmp3-dev zlib1g-dev libssl-dev

# Install ghcup
RUN a=$(arch); curl https://downloads.haskell.org/~ghcup/$a-linux-ghcup -o /usr/bin/ghcup && \
    chmod +x /usr/bin/ghcup

# Install ghc
RUN ghcup install ghc 9.6.3
# Install cabal
RUN ghcup install cabal 3.10.1.0
# Set both as default
RUN ghcup set ghc 9.6.3 && \
    ghcup set cabal 3.10.1.0

COPY . /project
WORKDIR /project

# Adjust PATH
ENV PATH="/root/.cabal/bin:/root/.ghcup/bin:$PATH"

# Adjust build
RUN cp ./scripts/cabal.project.local.linux ./cabal.project.local

# Compile simplex-chat
RUN cabal update
RUN cabal install

FROM scratch AS export-stage
COPY --from=build /root/.cabal/bin/simplex-chat /
