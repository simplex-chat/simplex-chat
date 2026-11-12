# syntax=docker/dockerfile:1

# release: libsimplex, the add-on and the npm packages pinned by package-lock.json
# source: all of them built from this checkout
ARG LIBS=release
ARG BACKEND=postgres
# The runtime glibc must be the same or newer.
ARG UBUNTU_LIBS=22.04
ARG GHC=9.6.3
ARG CABAL=3.10.2.0
ARG NODE=24

# --------------------------------------------------------------------------- #
# libsimplex from source
# --------------------------------------------------------------------------- #
FROM ubuntu:${UBUNTU_LIBS} AS libsimplex-source

ARG GHC
ARG CABAL
ARG BACKEND
ENV DEBIAN_FRONTEND=noninteractive

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
# The build script checks the export lists in these.
COPY libsimplex.dll.def flake.nix ./
COPY scripts/desktop ./scripts/desktop
COPY src ./src

# Other versions' build trees are removed: the script's simplex-chat-* glob
# fails with "cd: too many arguments" when the cache holds more than one.
RUN --mount=type=cache,target=/root/.cabal \
    --mount=type=cache,target=/src/dist-newstyle \
    set -eu; \
    ver=$(sed -n 's/^version: *//p' simplex-chat.cabal | tr -d '[:space:]'); \
    build_dir=dist-newstyle/build/$(uname -m)-linux/ghc-${GHC}/simplex-chat-$ver/build; \
    find dist-newstyle/build -maxdepth 3 -type d -name 'simplex-chat-*' \
      ! -path "*/simplex-chat-$ver" -exec rm -rf {} + 2>/dev/null || true; \
    cabal update; \
    SKIP_VLC=1 scripts/desktop/build-lib-linux.sh "${BACKEND}"; \
    mkdir -p /libs; \
    cp "$build_dir"/libsimplex.so "$build_dir"/deps/* /libs/

# --------------------------------------------------------------------------- #
# libs-source: libsimplex, the add-on and the npm packages from this checkout
# --------------------------------------------------------------------------- #
FROM node:${NODE}-bookworm-slim AS libs-source

ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update && apt-get install -y --no-install-recommends \
        build-essential python3 && \
    rm -rf /var/lib/apt/lists/*

COPY --from=libsimplex-source /libs /opt/simplex/libs

WORKDIR /src
COPY packages/simplex-chat-client/types/typescript packages/simplex-chat-client/types/typescript
RUN cd packages/simplex-chat-client/types/typescript && \
    npm install --no-audit --no-fund && \
    npx tsc && \
    mkdir -p /opt/simplex/packages && \
    npm pack --pack-destination /opt/simplex/packages

COPY packages/simplex-chat-nodejs packages/simplex-chat-nodejs
RUN cd packages/simplex-chat-nodejs && \
    npm install --no-audit --no-fund && \
    npm install --no-save --no-audit --no-fund /opt/simplex/packages/*.tgz && \
    npx node-gyp rebuild && \
    npx tsc && \
    cp src/simplex.* dist/ && \
    cp build/Release/simplex.node /opt/simplex/simplex.node && \
    npm pack --pack-destination /opt/simplex/packages

# --------------------------------------------------------------------------- #
# libs-release: libsimplex and the add-on pinned by the published simplex-chat
# --------------------------------------------------------------------------- #
FROM node:${NODE}-bookworm-slim AS libs-release

ARG BACKEND

WORKDIR /src/apps/simplex-support-bot
COPY apps/simplex-support-bot/package.json apps/simplex-support-bot/package-lock.json ./
RUN npm ci --no-audit --no-fund && \
    XDG_CACHE_HOME=/tmp/cache npx simplex-chat install --backend "${BACKEND}" && \
    mkdir -p /opt/simplex/packages && \
    cp -r "$(dirname "$(find /tmp/cache -name libsimplex.so)")" /opt/simplex/libs && \
    cp "$(find /tmp/cache -name '*.node')" /opt/simplex/simplex.node

FROM libs-${LIBS} AS libs

# --------------------------------------------------------------------------- #
# build
# --------------------------------------------------------------------------- #
FROM node:${NODE}-bookworm-slim AS build

COPY --from=libs /opt/simplex /opt/simplex

WORKDIR /src/apps/simplex-support-bot
COPY apps/simplex-support-bot/package.json apps/simplex-support-bot/package-lock.json ./
RUN npm ci --no-audit --no-fund && \
    find /opt/simplex/packages -name '*.tgz' -print0 | \
      xargs -0 -r npm install --no-save --no-audit --no-fund

COPY apps/simplex-support-bot ./
RUN npm run build && npm test

# --------------------------------------------------------------------------- #
# runtime
# --------------------------------------------------------------------------- #
FROM node:${NODE}-bookworm-slim AS runtime

ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update && apt-get install -y --no-install-recommends \
        ca-certificates dumb-init libffi8 libgmp10 libnuma1 libpq5 libssl3 zlib1g && \
    rm -rf /var/lib/apt/lists/*

# A named volume takes this ownership; a bind mount must be owned by uid 1000.
RUN mkdir -p /data && chown node:node /data && chmod 0700 /data

COPY --from=libs /opt/simplex/libs /opt/simplex/libs
COPY --from=libs /opt/simplex/simplex.node /opt/simplex/simplex.node
COPY --from=build --chown=node:node /src/apps/simplex-support-bot /src/apps/simplex-support-bot
# The library loads these instead of downloading libsimplex and the add-on at startup.
ENV SIMPLEX_LIBS_DIR=/opt/simplex/libs \
    SIMPLEX_ADDON_PATH=/opt/simplex/simplex.node

USER node
WORKDIR /src/apps/simplex-support-bot

ENV NODE_ENV=production
# Single API responses grow with the number of chats.
ENV NODE_OPTIONS=--max-old-space-size=8192

# node as PID 1 ignores SIGTERM, and the bot deletes the team group invite link on SIGTERM.
ENTRYPOINT ["dumb-init", "--", "node", "dist/index.js"]
CMD ["--config", "/etc/support-bot/config.toml"]
