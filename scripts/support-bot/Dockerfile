# syntax=docker/dockerfile:1
#
# Built from the repository root:
#
#   docker compose -f scripts/support-bot/compose.yml build
#   docker build -f scripts/support-bot/Dockerfile -t simplex-support-bot .
#
# libsimplex, the node library and the types all come from this tree. The first
# stage is a full GHC build of simplexmq and simplex-chat: hours on a cold cache,
# and it needs ~15 GB.

ARG UBUNTU=24.04
# A lib built here has to load on a runtime with the same glibc or newer.
ARG UBUNTU_LIBS=22.04
ARG GHC=9.6.3
ARG CABAL=3.10.2.0
ARG NODE=24
# The core, the node library and the bot must agree on it.
ARG SIMPLEX_BACKEND=postgres

# --------------------------------------------------------------------------- #
# libsimplex
# --------------------------------------------------------------------------- #
FROM ubuntu:${UBUNTU_LIBS} AS libsimplex

ARG GHC
ARG CABAL
ARG SIMPLEX_BACKEND
ENV DEBIAN_FRONTEND=noninteractive

# The toolchain the libs job in .github/workflows/build.yml uses, without the
# Java and Android SDK layers of Dockerfile.build, which the lib does not need.
RUN apt-get update && apt-get install -y --no-install-recommends \
        build-essential ca-certificates curl git libgmp3-dev libnuma-dev \
        libpq-dev libsqlite3-dev libssl-dev llvm patchelf pkg-config zlib1g-dev && \
    rm -rf /var/lib/apt/lists/*

ENV BOOTSTRAP_HASKELL_NONINTERACTIVE=1 \
    BOOTSTRAP_HASKELL_GHC_VERSION=${GHC} \
    BOOTSTRAP_HASKELL_CABAL_VERSION=${CABAL} \
    BOOTSTRAP_HASKELL_INSTALL_NO_STACK=true \
    BOOTSTRAP_HASKELL_INSTALL_NO_STACK_HOOK=true
RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
ENV PATH="/root/.ghcup/bin:/root/.cabal/bin:$PATH"
ENV CABAL_DIR=/root/.cabal

WORKDIR /src
COPY cabal.project simplex-chat.cabal README.md PRIVACY.md ./
COPY scripts/cabal.project.local.linux ./cabal.project.local
# libsimplex.dll.def and flake.nix hold the export lists the build script checks.
COPY libsimplex.dll.def flake.nix ./
COPY scripts/desktop ./scripts/desktop
COPY src ./src

# The VLC download the script ends with is for the desktop app, and it would run
# after the Haskell build.
RUN sed -i '/prepare-vlc-linux.sh/d' scripts/desktop/build-lib-linux.sh

# Cache mounts, not layers: the cabal store and the build tree survive a source
# change. build-lib-linux.sh is the command the libs job runs, and the two files
# copied out are what that job publishes. Build trees of other versions are
# removed first: the script's unquoted simplex-chat-* glob fails with "cd: too
# many arguments" when the cache mount holds more than one.
RUN --mount=type=cache,target=/root/.cabal \
    --mount=type=cache,target=/src/dist-newstyle \
    set -eu; \
    ver=$(sed -n 's/^version: *//p' simplex-chat.cabal | tr -d '[:space:]'); \
    build_dir=dist-newstyle/build/$(uname -m)-linux/ghc-${GHC}/simplex-chat-$ver/build; \
    find dist-newstyle/build -maxdepth 3 -type d -name 'simplex-chat-*' \
      ! -path "*/simplex-chat-$ver" -exec rm -rf {} + 2>/dev/null || true; \
    cabal update; \
    scripts/desktop/build-lib-linux.sh "${SIMPLEX_BACKEND}"; \
    mkdir -p /libs; \
    cp "$build_dir"/libsimplex.so "$build_dir"/deps/* /libs/

# --------------------------------------------------------------------------- #
# build
# --------------------------------------------------------------------------- #
FROM node:${NODE}-bookworm-slim AS build

ARG SIMPLEX_BACKEND
ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update && apt-get install -y --no-install-recommends \
        build-essential python3 && \
    rm -rf /var/lib/apt/lists/*

COPY --from=libsimplex /libs /opt/simplex/libs
# Needed by the bot's install too: npm re-runs the linked library's preinstall,
# which downloads the released libs without it.
ENV SIMPLEX_LIBS_DIR=/opt/simplex/libs
ENV SIMPLEX_BACKEND=${SIMPLEX_BACKEND}

WORKDIR /src
COPY packages/simplex-chat-client/types/typescript packages/simplex-chat-client/types/typescript
RUN cd packages/simplex-chat-client/types/typescript && \
    npm install --no-audit --no-fund && \
    npx tsc

COPY packages/simplex-chat-nodejs packages/simplex-chat-nodejs
RUN cd packages/simplex-chat-nodejs && \
    npm install --no-audit --no-fund && \
    npm install --no-save --no-audit --no-fund ../simplex-chat-client/types/typescript && \
    npx tsc && \
    cp src/simplex.* dist/

WORKDIR /src/apps/simplex-support-bot
COPY apps/simplex-support-bot/package.json apps/simplex-support-bot/package-lock.json ./
RUN npm ci --no-audit --no-fund
RUN npm install --no-save --no-audit --no-fund \
      /src/packages/simplex-chat-nodejs \
      /src/packages/simplex-chat-client/types/typescript

COPY apps/simplex-support-bot ./
RUN npm run build && npm test

# --------------------------------------------------------------------------- #
# runtime
# --------------------------------------------------------------------------- #
FROM node:${NODE}-bookworm-slim AS runtime

ARG SIMPLEX_BACKEND
ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update && apt-get install -y --no-install-recommends \
        ca-certificates dumb-init libgmp10 libnuma1 libpq5 && \
    rm -rf /var/lib/apt/lists/*

# A named volume at /data takes this ownership and mode; a bind mount instead
# keeps the host directory's, which then has to be owned by uid 1000.
RUN mkdir -p /data && chown node:node /data && chmod 0700 /data

COPY --from=build --chown=node:node /src /src

USER node
WORKDIR /src/apps/simplex-support-bot

ENV NODE_ENV=production
# Read by detectBackend(); must match the libsimplex built above.
ENV SIMPLEX_BACKEND=${SIMPLEX_BACKEND}
# Raised: single API responses grow with the number of chats.
ENV NODE_OPTIONS=--max-old-space-size=8192

# Exec form: under `sh -c` the bot would not receive SIGTERM, so it would not
# delete the team group invite link on shutdown.
ENTRYPOINT ["dumb-init", "--", "node", "dist/index.js"]
CMD ["--team-group", "Support Team", "--state-file", "/data/state.json"]
