# syntax=docker/dockerfile:1
# Build simplex-chat in relay mode with PostgreSQL support, then ship it on a
# slim runtime with only the libraries the binary links against. A bare
# `scratch` image cannot run this dynamically-linked binary.

ARG CHAT_REF=88df79d1e26921c7d1835fc8484fade596356fb6
ARG GHC=9.6.3
ARG CABAL=3.10.1.0

# ---- build ----------------------------------------------------------------
FROM ubuntu:22.04 AS build

ARG GHC
ARG CABAL
ENV DEBIAN_FRONTEND=noninteractive \
    PATH="/root/.cabal/bin:/root/.ghcup/bin:$PATH"

# Build toolchain and simplex-chat dependencies (libpq-dev for client_postgres).
RUN apt-get update && apt-get install -y --no-install-recommends \
      ca-certificates curl git build-essential \
      libpq-dev libgmp3-dev zlib1g-dev libnuma-dev libssl-dev \
      llvm-12 llvm-12-dev \
    && rm -rf /var/lib/apt/lists/*

# GHC + cabal via ghcup, then fetch the package index into this layer.
RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org \
      | BOOTSTRAP_HASKELL_NONINTERACTIVE=1 \
        BOOTSTRAP_HASKELL_GHC_VERSION="${GHC}" \
        BOOTSTRAP_HASKELL_CABAL_VERSION="${CABAL}" sh \
    && ghcup set ghc "${GHC}" \
    && ghcup set cabal "${CABAL}" \
    && cabal update

# CHAT_REF is declared here, after the toolchain layers, so changing the ref
# reuses everything above and rebuilds only the fetch + compile below. The
# fetch form accepts a commit hash as well as a tag or branch.
ARG CHAT_REF
WORKDIR /project
RUN git init -q . \
    && git remote add origin https://github.com/simplex-chat/simplex-chat \
    && git fetch -q --depth 1 origin "${CHAT_REF}" \
    && git checkout -q FETCH_HEAD

# Compile the executable with PostgreSQL persistence and strip it. The cabal
# store (compiled dependencies) and dist-newstyle are cache mounts, so a
# different ref recompiles only changed code, not every dependency.
RUN --mount=type=cache,target=/root/.cabal/store,sharing=locked \
    --mount=type=cache,target=/project/dist-newstyle,sharing=locked \
    cp scripts/cabal.project.local.linux cabal.project.local \
    && cabal build -fclient_postgres exe:simplex-chat \
    && bin=$(find dist-newstyle -name simplex-chat -type f -executable | head -n1) \
    && install -m 0755 "$bin" /simplex-chat-relay \
    && strip /simplex-chat-relay

# ---- runtime --------------------------------------------------------------
FROM debian:stable-slim AS runtime

# python3 runs the entrypoint; the rest are the binary's shared libraries.
RUN apt-get update && apt-get install -y --no-install-recommends \
      ca-certificates python3 \
      libpq5 libgmp10 libssl3 zlib1g libnuma1 libffi8 \
    && rm -rf /var/lib/apt/lists/* \
    && useradd -m -u 1000 relay

COPY --from=build /simplex-chat-relay /usr/local/bin/simplex-chat-relay
COPY entrypoint.py /usr/local/bin/entrypoint.py

# Run as non-root; host bind-mounts must be writable by this UID.
USER relay

# The relay (GHC runtime) shuts down cleanly on SIGINT, not SIGTERM.
STOPSIGNAL SIGINT
ENTRYPOINT ["python3", "/usr/local/bin/entrypoint.py"]
