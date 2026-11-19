# SimpleX channel for OpenClaw

Direct messages between you and an OpenClaw agent over SimpleX: end-to-end encrypted, with no phone number or user ID.

Version 0.1: text in direct chats. Replies are sent as plain text, split at 3,000 characters. Files, groups, streaming and Markdown rendering are not included yet.

## How it works

- The plugin runs the SimpleX chat core in the gateway process through the `simplex-chat` Node library.
- The chat database is stored in `<state dir>/simplex/` in the OpenClaw state volume.
- On start, the plugin creates a bot profile and a SimpleX address with automatic acceptance, and logs the address.
- A new contact's first message receives a pairing code; the operator approves it with `openclaw pairing approve simplex <CODE>`.
- Outbound targets are SimpleX contact ids: `simplex:<contact id>` or `<contact id>`.

## Install with Docker

The library builds a native add-on, and OpenClaw installs plugin dependencies without running install scripts. The plugin is therefore built into an image derived from the OpenClaw image:

```
docker build -t openclaw-simplex --build-arg OPENCLAW_IMAGE=<the OpenClaw image you run> packages/simplex-chat-openclaw
```

Recreate the gateway container from `openclaw-simplex` in place of the OpenClaw image, keeping every other option. Then:

```
docker exec -it openclaw-gateway openclaw plugins install --link /opt/simplex-chat-openclaw --force --accept-capabilities
docker exec -it openclaw-gateway openclaw config set --batch-json '[{"path":"channels.simplex","value":{"enabled":true,"name":"OpenClaw"}}]'
docker logs openclaw-gateway 2>&1 | grep "SimpleX address"
```

Connect to the printed address in the SimpleX app and send a message. The reply contains a pairing code and your contact id:

```
docker exec -it openclaw-gateway openclaw pairing approve simplex <CODE>
```

## Config

`channels.simplex`:

```
enabled     boolean, default true
name        bot display name, default "OpenClaw"
dmPolicy    pairing | allowlist | open | disabled, default pairing
allowFrom   contact ids allowed to message the agent
```

## Scheduled delivery

```
openclaw cron create "0 7 * * *" "<prompt>" --name "Digest" --announce --channel simplex --to <contact id>
```
