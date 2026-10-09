ARG OPENCLAW_IMAGE=ghcr.io/openclaw/openclaw:latest
FROM ${OPENCLAW_IMAGE}

USER root
RUN apt-get update \
  && apt-get install -y --no-install-recommends python3 make g++ \
  && rm -rf /var/lib/apt/lists/*

COPY --chown=node:node package.json openclaw.plugin.json index.ts /opt/simplex-chat-openclaw/
COPY --chown=node:node src /opt/simplex-chat-openclaw/src

USER node
WORKDIR /opt/simplex-chat-openclaw
RUN npm install --omit=dev --omit=peer --allow-scripts=simplex-chat --no-audit --no-fund

WORKDIR /app
