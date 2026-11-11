# syntax=docker/dockerfile:1
# Built from the repository root, not this directory, because the COPY paths are repo-relative.
# The Haskell stage compiles simplexmq and simplex-chat from source, which takes hours on a cold
# cache and needs about 15 GB.

ARG UBUNTU=24.04
ARG GHC=9.6.3
ARG CABAL=3.10.2.0
ARG NODE=24

# The checkout page's dist/ is gitignored, so this stage builds it.
FROM node:${NODE}-bookworm-slim AS web

WORKDIR /web
COPY apps/simplex-badge-service/web/package.json apps/simplex-badge-service/web/package-lock.json ./
RUN npm ci
COPY apps/simplex-badge-service/web ./
RUN npm run build

FROM ubuntu:${UBUNTU} AS build

ARG GHC
ARG CABAL
ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update && apt-get install -y --no-install-recommends \
        build-essential ca-certificates curl git libgmp3-dev libnuma-dev \
        libpq-dev libssl-dev llvm pkg-config zlib1g-dev && \
    rm -rf /var/lib/apt/lists/*

ENV BOOTSTRAP_HASKELL_NONINTERACTIVE=1 \
    BOOTSTRAP_HASKELL_GHC_VERSION=${GHC} \
    BOOTSTRAP_HASKELL_CABAL_VERSION=${CABAL} \
    BOOTSTRAP_HASKELL_INSTALL_NO_STACK=true \
    BOOTSTRAP_HASKELL_INSTALL_NO_STACK_HOOK=true
RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
ENV PATH="/root/.ghcup/bin:/root/.cabal/bin:$PATH"
# Set explicitly so the cache mount below matches where cabal keeps its store.
ENV CABAL_DIR=/root/.cabal

WORKDIR /src
COPY cabal.project simplex-chat.cabal README.md PRIVACY.md ./
COPY scripts/cabal.project.local.linux ./cabal.project.local
COPY src ./src
# Excluding web/ keeps a webapp edit from invalidating this layer and re-running the cabal build,
# and the binary does not depend on it.
COPY --exclude=web apps/simplex-badge-service ./apps/simplex-badge-service

# The store and build tree are cache mounts rather than layers, so the binary is copied out inside
# the same RUN because dist-newstyle does not exist in the resulting layer.
RUN --mount=type=cache,target=/root/.cabal \
    --mount=type=cache,target=/src/dist-newstyle \
    set -eu; \
    cabal update; \
    cabal build --flags="+client_postgres" exe:simplex-badge-service; \
    mkdir -p /out; \
    cp "$(cabal list-bin --flags="+client_postgres" exe:simplex-badge-service)" /out/simplex-badge-service; \
    strip /out/simplex-badge-service

FROM ubuntu:${UBUNTU} AS runtime

ENV DEBIAN_FRONTEND=noninteractive

# Built with +client_postgres, so the binary needs libpq and there is no SQLite backend.
RUN apt-get update && apt-get install -y --no-install-recommends \
        ca-certificates dumb-init libgmp10 libpq5 libssl3 zlib1g && \
    rm -rf /var/lib/apt/lists/*

# Set these to your own uid and gid so the state volume is readable without root.
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

COPY --from=build /out/simplex-badge-service /usr/local/bin/simplex-badge-service
COPY --from=web /web/dist /srv/web

USER badge
WORKDIR /home/badge

EXPOSE 8080

# There is no HEALTHCHECK because the listener answers before the chat side has an address, so it
# would report healthy too early.

# The flags live in CMD so `docker compose run` can replace them while dumb-init still forwards
# signals.
ENTRYPOINT ["dumb-init", "--", "simplex-badge-service"]
# Without --yes-migrate the service blocks on stdin at every schema change, and a container has no
# console to answer.
CMD ["-d", "postgresql://simplex@127.0.0.1:5432/badge_service", "--create-schema", "--yes-migrate", "--service-config", "/etc/simplex-badge-service/badge_service.ini"]
