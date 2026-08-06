# SimpleX badge service

Scaffolding for the SimpleX supporter-badge RPC service. The wire protocol is specified in [`docs/protocol/badges-rpc.md`](../../docs/protocol/badges-rpc.md), and the implementation plans live under [`plans/`](../../plans) (`2026-07-30-supporter-badges-v3-ux.md`, `2026-07-31-badges-core-implementation.md`, `2026-08-04-badges-mvp-scope.md`).

At this stage the service:

- creates a double-ratchet contact address on first start (service RPC requires DR, see [`docs/protocol/badges-rpc.md`](../../docs/protocol/badges-rpc.md)),
- listens for service requests (`CEvtServiceRequest`) on that address and responds to every command with `unsupported_version`,
- does not accept contact requests — the address is for RPC only,
- exposes a placeholder schema migration (`sx_badge_service_test`) and its own migrations table (`sx_badge_service_migrations`).

Business logic — command dispatch, ledger writes, credential signing, provider webhooks — is left for follow-up per the plans.

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
