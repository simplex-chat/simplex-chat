# SimpleX names on a local chain

The app test backend of desktop, Android and iOS adds `.simplex` names on a local chain, so that the names screens' scripted cases test against the names contracts, simplexmq's resolver and the store service. The `.testing` names of each platform's `names-resolver.py` stay for the cases that use them.

## The build: `build.sh <package dir> <simplexmq clone>`

Both platforms' `build-backend.sh` build `simplex-store-service` with the other binaries, then call `build.sh` with their package folder and the simplexmq clone their `smp-server` is built from. It writes `names-chain/` beside the binaries:

| Path | What | Pinned by |
|---|---|---|
| `anvil` | Foundry's anvil, from the release tarball for Linux x86-64 or macOS arm64 | `ARG FOUNDRY_VERSION=` of `apps/simplex-domains/webapp/dev/Dockerfile` |
| `contracts/artifacts/contracts/` | the names contracts compiled as that Dockerfile compiles them: simplex-namespace-contract, its `ens-contracts`, `pnpm@9 install --frozen-lockfile --ignore-scripts`, `hardhat compile` | `ARG NAMESPACE_COMMIT=` and `ARG ENS_CONTRACTS_COMMIT=` |
| `test/fixtures/terms/` | the contract repository's terms texts, which `dev/deploy.ts` links | `ARG NAMESPACE_COMMIT=` |
| `webapp/` | `package.json`, `package-lock.json`, `src/` and `dev/` of the site's webapp, with `npm ci` | the `apps/simplex-domains` submodule's commit |
| `resolver/snrc-resolve.py`, `resolver/lib/` | simplexmq's resolver, and `eth-hash[pycryptodome]` and `pycryptodome` installed with `pip install --target` | the commit `cabal.project` pins, and the versions of its `scripts/resolver/service/uv.lock` |

`build.sh` repeats none of these pins. It initializes the submodule itself, as the backend jobs check out without submodules. It needs Node 24, which the workflows set up with `actions/setup-node`, and a `python3` of 3.11 or newer, which it checks first. Each workflow names the backend archive after the hash of its sources, the store service's and `build.sh` among them, followed by the submodule's commit, so a change of either rebuilds it.

## The start: `backend.sh start <bin dir> <work dir> <server host>`

Each platform's `backend.sh` calls it last, with its unpacked archive and work folder: desktop and Android with the host `127.0.0.1`, iOS with `localhost`. It starts, in order:

| Part | Where | What |
|---|---|---|
| anvil | 127.0.0.1:8545 | chain 31337, a block a second |
| terms server | 127.0.0.1:8098 | the site's `dev/serve-terms.ts`, serving the terms texts the deployment links |
| deployment | | the site's `dev/deploy.ts` with a 5-second commitment age, written to `chain.json` |
| resolver | 127.0.0.1:18001 | `snrc-resolve.py`, one process, for the `.simplex` contracts of `chain.json` |
| second SMP server | port 5224 | `smp-server` with `[NAMES]` on and `resolver_endpoint` the resolver; it listens as the platform's first server on 5223 does |
| store service | 127.0.0.1:18002 | `simplex-store-service` with its SQLite database `store-service`, `serve_webapp = off` (its API, names included, on this port alone), `[names]` on the chain with 100 link edits a year, `[chain]` with 1 confirmation |

The store service starts once to make its database and its SimpleX address, which the start reads from the database, and stops; `issue-codes.py` then types `//issue name 6` three times, `//issue name 7 years 5` and `//issue name 8` into its `--run-cli` console, and the service starts again.

The deployment seeds `bakery` (a contact and a channel link), `harbour` (registered, without links) and the community name `bakehouse`; it reserves `support`, `simplex` and `official`, and lists `bakers` and `bakehouse` as community names. The start then registers `ciowner.simplex` with the first issued code through the service's order API, held by a new name code: `order.mjs register` signs the order, the reveal and the terms with the site's own `src/flow.ts` and `src/api.ts`. A CLI peer `ci_owner` creates its address on the second server, `order.mjs set-link` makes that address the name's contact link, and `ci_owner` claims the name with `/_set domain`, asked again for up to 60 seconds, since the core checks the claim against the name's link through the second server.

The start fails, and the platform's start step with it, unless:

- a CLI client whose only server is the second one answers `/_connect plan` for `cifreename.simplex` with "SimpleX name cifreename.simplex is available: …" and for `harbour.simplex` with "SimpleX name harbour.simplex has no valid connection link";
- `POST /api/code/check` answers the second issued code with `"state":"unused"`;
- that client answers `/_connect plan` for `ciowner.simplex` with "contact address: ok to connect", asked again for up to 60 seconds.

## Outputs

In the work folder:

| File | Holds |
|---|---|
| `names-chain-server` | the second SMP server's address, `smp://<fingerprint>@<host>:5224` |
| `store-service` | the store service's SimpleX address |
| `name-codes.json` | the issued codes left for tests: `code`, `minLength`, `years` each |
| `name-code.json` | `ciowner.simplex`: `name`, `label`, `nameCode`, `registrationId` |
| `chain.json` | the deployment: the contracts' addresses, the terms, the test accounts and their keys |

The start ends by printing `ls -l` of these files and their contents. The logs are `chain-anvil.log`, `chain-terms.log`, `chain-deploy.log`, `chain-resolver.log`, `chain-smp.log`, `store-service.log`, `store-service-cli.log`, `chain_probe-cli.log` and `chain_owner-cli.log`, uploaded with the platform's `*.log`; `backend.sh stop <work dir>`, which the platform's stop calls, stops every part and prints the logs.

## Editing the name's link: `backend.sh set-link <work dir> <address>`

It runs `order.mjs set-link`, which signs a contact link edit of the name in `name-code.json` with its name code and sends it to the store service, which relays it to the chain; it prints the transaction. The address is a short contact address (`https://<host>/a#…`). Each edit counts towards the name's 100 a year; the start spends one.

## Keys and requirements

The keys are public test keys: anvil's development mnemonic, which `dev/deploy.ts` uses, and the dev issuer key of `scripts/store-service/dev/chain.sh` at index 999, which the store service accepts only on chain 31337. The codes and the name code are test values on a local chain, printed in public logs. Every part but the second SMP server listens on 127.0.0.1 only.

The start needs Node 24 (`actions/setup-node` in each job that starts the backend), a `python3` of 3.11 or newer for the resolver, `jq`, `curl`, `nc` and `sqlite3`. Android forwards the emulator's port 5224 to the runner's (`emulator.sh reverse 5224`), as it does 5223.
