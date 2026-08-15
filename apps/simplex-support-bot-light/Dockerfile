# syntax=docker/dockerfile:1
#
# Built from the repository root, not this directory: the image is made from the
# Haskell core and the Python library in this tree, neither of them released.
#
#   docker compose build          # from apps/simplex-support-bot-light
#   docker build -f apps/simplex-support-bot-light/Dockerfile .
#
# The first stage compiles libsimplex from src/. That is a full GHC build of
# simplexmq and simplex-chat: hours on a cold cache, and it needs ~15 GB.

ARG UBUNTU=24.04
# The released libs are built on 22.04; a lib built here has to load on a runtime
# with the same glibc or newer, not the other way round.
ARG UBUNTU_LIBS=22.04
ARG GHC=9.6.3
ARG CABAL=3.10.2.0

# --------------------------------------------------------------------------- #
# libsimplex — the cabal invocation of scripts/desktop/build-lib-linux.sh, which
# is what produces the .so the published libs archive is repackaged from.
# --------------------------------------------------------------------------- #
FROM ubuntu:${UBUNTU_LIBS} AS libsimplex

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

# Cache mounts, not layers: the Haskell store and the build tree survive a
# source change, which is the difference between minutes and hours. The RTS and
# package libraries are copied next to libsimplex.so because its rpath is $ORIGIN.
RUN --mount=type=cache,target=/root/.cabal \
    --mount=type=cache,target=/src/dist-newstyle \
    set -eu; \
    cabal update; \
    cabal build lib:simplex-chat \
      --ghc-options='-optl-Wl,-rpath,$ORIGIN -optl-Wl,-soname,libsimplex.so -flink-rts -threaded' \
      --constraint 'simplexmq +client_library' \
      --constraint 'simplex-chat +client_library'; \
    lib=$(ls -t /src/dist-newstyle/build/*/ghc-${GHC}/simplex-chat-*/build/libHSsimplex-chat-*-inplace-ghc${GHC}.so | head -1); \
    build_dir=$(dirname "$lib"); \
    mv "$lib" "$build_dir/libsimplex.so"; \
    mkdir -p /libs; \
    ldd "$build_dir/libsimplex.so" | grep ghc | cut -d' ' -f 3 | xargs -I {} cp {} /libs/; \
    cp "$build_dir/libsimplex.so" /libs/

# --------------------------------------------------------------------------- #
# the bot
# --------------------------------------------------------------------------- #
# libsimplex is a glibc build and will not load on musl, and it is compiled
# against this image's libraries in the stage above.
FROM ubuntu:${UBUNTU}

ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update && apt-get install -y --no-install-recommends \
        ca-certificates curl dumb-init libffi8 libgmp10 libnuma1 && \
    rm -rf /var/lib/apt/lists/*

RUN curl -LsSf https://astral.sh/uv/install.sh | sh && \
    mv /root/.local/bin/uv /usr/local/bin/uv

# The ids that must own ./state on the host; a bind mount keeps host ownership.
# Build with your own to avoid needing root to read the bot's state:
#   USER_UID=$(id -u) USER_GID=$(id -g) docker compose build
ARG USER_UID=1000
ARG USER_GID=1000

# ubuntu:24.04 ships a default user at 1000, so drop whoever holds the ids.
RUN if existing_user=$(getent passwd ${USER_UID} | cut -d: -f1) && [ -n "${existing_user}" ]; then \
        userdel -r "${existing_user}" 2>/dev/null || userdel "${existing_user}"; \
    fi && \
    if existing_group=$(getent group ${USER_GID} | cut -d: -f1) && [ -n "${existing_group}" ]; then \
        groupdel "${existing_group}" 2>/dev/null || true; \
    fi && \
    groupadd -g ${USER_GID} supportbot && \
    useradd -u ${USER_UID} -g ${USER_GID} -m -d /home/supportbot supportbot

# Applies only when /data is not bind-mounted; a bind mount keeps the host
# directory's ownership and mode.
RUN mkdir -p /data && chown supportbot:supportbot /data && chmod 0700 /data

# Read by simplex_chat._native instead of downloading a release, which is what
# makes the bot run against the core built above rather than the published one.
COPY --from=libsimplex /libs /opt/simplex/libs
ENV SIMPLEX_LIBS_DIR=/opt/simplex/libs

USER supportbot
WORKDIR /home/supportbot

ENV VIRTUAL_ENV=/home/supportbot/.venv
ENV PATH="$VIRTUAL_ENV/bin:$PATH"

# The library is installed from this tree: the APIs the bot uses
# (install_signal_handlers, sync_profile, api_merge_*_custom_data) are unreleased.
COPY --chown=supportbot:supportbot packages/simplex-chat-python /home/supportbot/simplex-chat-python
RUN uv venv --python 3.12 "$VIRTUAL_ENV" && \
    uv pip install /home/supportbot/simplex-chat-python

COPY --chown=supportbot:supportbot apps/simplex-support-bot-light /home/supportbot/app
RUN uv pip install -e /home/supportbot/app

ENV PYTHONUNBUFFERED=1

# No HEALTHCHECK: /health is published for an external monitor, and a container
# check would restart a bot whose chat controller is merely slow.

# Exec form: shell form would run under `sh -c`, which does not forward SIGTERM
# to the bot, so the graceful-stop path in __main__.py would never fire.
ENTRYPOINT ["dumb-init", "--", "support-bot-light", "--config", "/etc/support-bot-light/config.toml"]
