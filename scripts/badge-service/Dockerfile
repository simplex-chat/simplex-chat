# syntax=docker/dockerfile:1
#
# Built from the repository root, not this directory:
#
#   docker compose -f scripts/badge-service/compose.yml build
#   docker build -f scripts/badge-service/Dockerfile -t simplex-badge-service .
#
# The Haskell stage compiles simplexmq and simplex-chat from source: hours on a
# cold cache, and it needs ~15 GB.

ARG UBUNTU=24.04
ARG GHC=9.6.3
ARG CABAL=3.10.2.0
ARG NODE=24

# --------------------------------------------------------------------------- #
# web — the checkout page served from static_dir. Not committed: dist/ is
# gitignored, so the image has to build it.
# --------------------------------------------------------------------------- #
FROM node:${NODE}-bookworm-slim AS web

WORKDIR /web
COPY apps/simplex-badge-service/web/package.json apps/simplex-badge-service/web/package-lock.json ./
RUN npm ci
COPY apps/simplex-badge-service/web ./
RUN npm run build

# --------------------------------------------------------------------------- #
# build — the service executable
# --------------------------------------------------------------------------- #
FROM ubuntu:${UBUNTU} AS build

ARG GHC
ARG CABAL
ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update && apt-get install -y --no-install-recommends \
        build-essential ca-certificates curl git libgmp3-dev libnuma-dev \
        libsqlite3-dev libssl-dev llvm pkg-config zlib1g-dev && \
    rm -rf /var/lib/apt/lists/*

ENV BOOTSTRAP_HASKELL_NONINTERACTIVE=1 \
    BOOTSTRAP_HASKELL_GHC_VERSION=${GHC} \
    BOOTSTRAP_HASKELL_CABAL_VERSION=${CABAL} \
    BOOTSTRAP_HASKELL_INSTALL_NO_STACK=true \
    BOOTSTRAP_HASKELL_INSTALL_NO_STACK_HOOK=true
RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
ENV PATH="/root/.ghcup/bin:/root/.cabal/bin:$PATH"
# Explicit, so the cache mount below is where cabal actually keeps its store.
ENV CABAL_DIR=/root/.cabal

WORKDIR /src
COPY cabal.project simplex-chat.cabal README.md PRIVACY.md ./
COPY scripts/cabal.project.local.linux ./cabal.project.local
COPY src ./src
COPY apps/simplex-badge-service ./apps/simplex-badge-service

# Cache mounts, not layers: the Haskell store and the build tree survive a
# source change, which is the difference between minutes and hours. The binary
# is copied out inside the same RUN because dist-newstyle is a cache mount and
# does not exist in the resulting layer.
RUN --mount=type=cache,target=/root/.cabal \
    --mount=type=cache,target=/src/dist-newstyle \
    set -eu; \
    cabal update; \
    cabal build exe:simplex-badge-service; \
    mkdir -p /out; \
    cp "$(cabal list-bin exe:simplex-badge-service)" /out/simplex-badge-service; \
    strip /out/simplex-badge-service

# --------------------------------------------------------------------------- #
# runtime
# --------------------------------------------------------------------------- #
FROM ubuntu:${UBUNTU} AS runtime

ENV DEBIAN_FRONTEND=noninteractive

# The set `ldd` reports for the built binary: libm, libz, libcrypto, libgmp,
# libc. sqlcipher is linked in, so there is no libsqlite3 here.
RUN apt-get update && apt-get install -y --no-install-recommends \
        ca-certificates dumb-init libgmp10 libssl3 zlib1g && \
    rm -rf /var/lib/apt/lists/*

# The ids that must own the state volume. Build with your own to read it
# without root:  USER_UID=$(id -u) USER_GID=$(id -g) docker compose build
ARG USER_UID=1000
ARG USER_GID=1000

# ubuntu:24.04 ships a default user at 1000, so drop whoever holds the ids.
RUN if existing_user=$(getent passwd ${USER_UID} | cut -d: -f1) && [ -n "${existing_user}" ]; then \
        userdel -r "${existing_user}" 2>/dev/null || userdel "${existing_user}"; \
    fi && \
    if existing_group=$(getent group ${USER_GID} | cut -d: -f1) && [ -n "${existing_group}" ]; then \
        groupdel "${existing_group}" 2>/dev/null || true; \
    fi && \
    groupadd -g ${USER_GID} badge && \
    useradd -u ${USER_UID} -g ${USER_GID} -m -d /home/badge badge

# Holds the chat and agent databases, the service address, and every issued
# code hash. Deleting it means a new address and codes that can never be redeemed.
RUN mkdir -p /data && chown badge:badge /data && chmod 0700 /data

COPY --from=build /out/simplex-badge-service /usr/local/bin/simplex-badge-service
COPY --from=web /web/dist /srv/web

USER badge
WORKDIR /home/badge

EXPOSE 8080

# No HEALTHCHECK: the listener answers before the chat side has an address, so a
# check on it would report healthy too early and restart nothing useful.

# Exec form, and the flags in CMD so `docker compose run` can replace them
# without losing dumb-init's signal forwarding.
ENTRYPOINT ["dumb-init", "--", "simplex-badge-service"]
# --yes-migrate: a container has no console to answer the migration prompt on, so without
# it the service blocks on stdin at every schema change.
CMD ["-d", "/data/badge_service", "--yes-migrate", "--service-config", "/etc/simplex-badge-service/badge_service.ini"]
