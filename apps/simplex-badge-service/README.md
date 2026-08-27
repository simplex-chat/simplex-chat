# SimpleX badge service

Scaffolding for the SimpleX supporter-badge RPC service. The wire protocol is specified in [`docs/protocol/badges-rpc.md`](../../docs/protocol/badges-rpc.md), and the implementation plans live under [`plans/`](../../plans) (`2026-07-30-supporter-badges-v3-ux.md`, `2026-07-31-badges-core-implementation.md`, `2026-08-04-badges-mvp-scope.md`).

At this stage the service:

- creates a double-ratchet contact address on first start (service RPC requires DR, see [`docs/protocol/badges-rpc.md`](../../docs/protocol/badges-rpc.md)),
- listens for service requests (`CEvtServiceRequest`) on that address and responds to every command with `unsupported_version`,
- does not accept contact requests — the address is for RPC only,
- exposes a placeholder schema migration (`sx_badge_service_test`) and its own migrations table (`sx_badge_service_migrations`).

Business logic — command dispatch, ledger writes, credential signing, provider webhooks — is left for follow-up per the plans.

## Site

With a `[web]` section in `badge_service.ini` the same process runs the checkout site on `[web]
port`, bound to `[web] host` (default `127.0.0.1`, so a default deployment is not exposed without
a reverse proxy). It serves `/` (the page), `/assets/<buildHash>/<name>` and `/api/catalog`.

The site is built by `web/` and its `dist/` is committed and embedded into the binary, so building
the service never needs node. Change anything under `web/src` or `web/assets` and run `npm run
build` in `web/`, committing the result — the running service serves the bytes that were embedded
when it was compiled.

`[web] web_dir` overrides that and serves the same URLs from a directory on disk (the `web/`
directory itself: `dist/` under it, plus `index.html` and `styles.css` beside it), re-read on
every request so an edit is visible on reload. **It is for front-end development only**: every
response is `no-store`, and each request re-reads and re-hashes the whole directory.

## Build

Build prerequisites and the general contribution flow are in [`docs/CONTRIBUTING.md`](../../docs/CONTRIBUTING.md).

```
cabal build exe:simplex-badge-service
```

## Run

```
simplex-badge-service --help
```

- default (no `--run-cli`): background service mode, no interactive terminal.
- `--run-cli`: interactive CLI that also processes service requests (mirrors `simplex-directory-service --run-cli`).
- `--no-address`: skip address creation on start-up (for operators who provision the address themselves).
