# libsimplex is a glibc build and will not load on musl.
FROM ubuntu:24.04

ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update && apt-get install -y --no-install-recommends \
        curl ca-certificates dumb-init && \
    rm -rf /var/lib/apt/lists/*

RUN curl -LsSf https://astral.sh/uv/install.sh | sh && \
    mv /root/.local/bin/uv /usr/local/bin/uv

# 1000:1000 must own ./state on the host; a bind mount keeps host ownership.
# ubuntu:24.04 ships a default user at those ids, so drop it first.
RUN if existing_user=$(getent passwd 1000 | cut -d: -f1) && [ -n "${existing_user}" ]; then \
        userdel -r "${existing_user}" 2>/dev/null || userdel "${existing_user}"; \
    fi && \
    if existing_group=$(getent group 1000 | cut -d: -f1) && [ -n "${existing_group}" ]; then \
        groupdel "${existing_group}" 2>/dev/null || true; \
    fi && \
    groupadd -g 1000 supportbot && \
    useradd -u 1000 -g 1000 -m -d /home/supportbot supportbot

# Applies only when /data is not bind-mounted; a bind mount keeps the host
# directory's ownership and mode.
RUN mkdir -p /data && chown supportbot:supportbot /data && chmod 0700 /data

USER supportbot
WORKDIR /home/supportbot

ENV VIRTUAL_ENV=/home/supportbot/.venv
ENV PATH="$VIRTUAL_ENV/bin:$PATH"

# Pinned because pyproject.toml declares a floor and there is no lockfile.
# 7.1.0b0 is the first release carrying the client APIs this bot uses
# (install_signal_handlers, sync_profile, api_accept_member_contact,
# api_merge_*_custom_data); it has to be on PyPI for this build to succeed.
# libsimplex is pre-fetched into the supportbot cache, before COPY so editing bot
# code does not re-download it, and after USER so the runtime user finds it.
RUN uv venv --python 3.12 "$VIRTUAL_ENV" && \
    uv pip install "simplex-chat==7.1.0b0" && \
    python -m simplex_chat install --backend sqlite

COPY --chown=supportbot:supportbot . /home/supportbot/app
RUN uv pip install -e /home/supportbot/app

ENV PYTHONUNBUFFERED=1

# No HEALTHCHECK: /health is published for an external monitor, and a container
# check would restart a bot whose chat controller is merely slow.

# Exec form: shell form would run under `sh -c`, which does not forward SIGTERM
# to the bot, so the graceful-stop path in __main__.py would never fire.
ENTRYPOINT ["dumb-init", "--", "support-bot-light", "--config", "/etc/support-bot-light/config.toml"]
