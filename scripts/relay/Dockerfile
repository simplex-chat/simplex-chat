# syntax=docker/dockerfile:1

ARG CHAT_REF=88df79d1e26921c7d1835fc8484fade596356fb6
ARG GHC=9.6.3
# 3.10.1.0 predates the Hackage root key rotation and fails `cabal update`.
ARG CABAL=3.10.2.0

# ---- build ----------------------------------------------------------------
FROM ubuntu:22.04 AS build

ARG GHC
ARG CABAL
ENV DEBIAN_FRONTEND=noninteractive \
    PATH="/root/.cabal/bin:/root/.ghcup/bin:$PATH"

RUN apt-get update && apt-get install -y --no-install-recommends \
      ca-certificates curl git build-essential \
      libpq-dev libgmp3-dev zlib1g-dev libnuma-dev libssl-dev \
      llvm-12 llvm-12-dev \
    && rm -rf /var/lib/apt/lists/*

RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org \
      | BOOTSTRAP_HASKELL_NONINTERACTIVE=1 \
        BOOTSTRAP_HASKELL_GHC_VERSION="${GHC}" \
        BOOTSTRAP_HASKELL_CABAL_VERSION="${CABAL}" sh \
    && ghcup set ghc "${GHC}" \
    && ghcup set cabal "${CABAL}"

# Kept out of the layer above so that a failure here does not reinstall GHC.
# Retried because a failed fetch falls back to the mirrors in Hackage's
# mirrors.json, which are dead, and the last one aborts the build.
RUN cabal update || { sleep 5; cabal update; } || { sleep 20; cabal update; }

# Declared after the toolchain layers so that changing the ref does not rebuild
# them. Unlike `clone --branch`, this form also accepts a commit hash.
ARG CHAT_REF
WORKDIR /project
RUN git init -q . \
    && git remote add origin https://github.com/simplex-chat/simplex-chat \
    && git fetch -q --depth 1 origin "${CHAT_REF}" \
    && git checkout -q FETCH_HEAD

# The cache mounts keep compiled dependencies across ref changes.
RUN --mount=type=cache,target=/root/.cabal/store,sharing=locked \
    --mount=type=cache,target=/project/dist-newstyle,sharing=locked \
    cp scripts/cabal.project.local.linux cabal.project.local \
    && cabal build -fclient_postgres exe:simplex-chat \
    && bin=$(find dist-newstyle -name simplex-chat -type f -executable | head -n1) \
    && install -m 0755 "$bin" /simplex-chat-relay \
    && strip /simplex-chat-relay

# ---- runtime --------------------------------------------------------------
FROM debian:stable-slim AS runtime

# The binary is dynamically linked, so it needs these libraries at runtime.
RUN apt-get update && apt-get install -y --no-install-recommends \
      ca-certificates python3 \
      libpq5 libgmp10 libssl3 zlib1g libnuma1 libffi8 \
    && rm -rf /var/lib/apt/lists/* \
    && useradd -m -u 1000 relay

COPY --from=build /simplex-chat-relay /usr/local/bin/simplex-chat-relay
COPY entrypoint.py /usr/local/bin/entrypoint.py

# Bind-mounted host directories must be writable by this UID.
USER relay

# The relay shuts down cleanly on SIGINT, not SIGTERM.
STOPSIGNAL SIGINT
ENTRYPOINT ["python3", "/usr/local/bin/entrypoint.py"]
