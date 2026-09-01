# SimpleX badge service

Scaffolding for the SimpleX supporter-badge RPC service. The wire protocol is specified in [`docs/protocol/badges-rpc.md`](../../docs/protocol/badges-rpc.md), and the implementation plans live under [`plans/`](../../plans) (`2026-07-30-supporter-badges-v3-ux.md`, `2026-07-31-badges-core-implementation.md`, `2026-08-04-badges-mvp-scope.md`).

At this stage the service:

- creates a double-ratchet contact address on first start (service RPC requires DR, see [`docs/protocol/badges-rpc.md`](../../docs/protocol/badges-rpc.md)),
- listens for service requests (`CEvtServiceRequest`) on that address, rejects a request whose `purchaseKey` is not the key the agent verified the signature against, and answers `redeemBadgeCode`,
- issues redemption codes, storing only their `SHA-256` and printing each code once,
- does not accept contact requests — the address is for RPC only,
- owns the `sx_badge_service_`-prefixed tables and its own migrations table (`sx_badge_service_migrations`).

Every other command still answers `unsupported_version`. Ledger writes, invoices and provider webhooks are left for follow-up per the plans; a redemption issues one credential and reports an empty statement.

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

The service cannot sign credentials without an issuer key and refuses to start without one:

- `--issuer-key-idx IDX` — the index the apps find the matching public key under (`badgePublicKeys` in `ChatConfig`).
- `--issuer-secret SECRET` — the issuer secret from `simplex-chat badge keygen`.

The service checks the secret against the configured public key at that index and refuses to start
if they disagree: credentials signed with the wrong key cannot be verified by any client, and the
codes redeemed against them would be spent for nothing.

## Issuing codes

Issuing a code is an operator command sent to the running service in `--run-cli` mode, not a way
to start it — so codes are issued without a second process touching the service's database:

```
//issue <badge_type> [months] [paid|unpaid|free]
//issue supporter 12
```

`months` defaults to 1 and must be between 1 and 255; the status defaults to `free` and records
whether the code was sold (`paid`), is awaiting payment (`unpaid`), or was issued by an operator
(`free`) — redemption never reads it.

The code is printed once and only its `SHA-256` is stored, so a code that is not copied when it is
shown cannot be recovered.

Core parses `//...` into `CustomChatCommand` and leaves it to the service's `preCmdHook`, which is
why issuing codes lives in the service rather than in core.
