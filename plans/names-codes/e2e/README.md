# End-to-end check against the contracts

`StoreE2E.test.ts` runs the store's flow on a local hardhat chain against ens-contracts PR #36 (`simplex-network/ens-contracts`, branch `ab/sponsor-sign-duration`), which builds on the merged PR #35 and adds the duration to the owner's `RegisterName` intent:
- the owner code round trip, its BIP-39 phrase, seed and account-0 wallet, and the commitment secret from the seed;
- the commitment with a record, `commit`, the owner's `RegisterName` intent (with its duration and nonce, and `revealBefore` as its deadline; a longer duration than signed reverts `InvalidSignature`), and `registerWithCredit` from the registrar while public sales are closed, spending exactly the list price from the allowance;
- a second order for the name reverting `NameNotAvailable`, and, with an expired intent, `SignatureExpired` instead while the chain's owner is someone else and `available(label)` is false, the case the service must still call taken;
- another browser with only the owner code and the registration view (`sealed`, `commitment`, `years`, resolver, sponsor, chain id, TLD, `intentNonce`, `revealBefore`): opening the sealed label and record values (AES-256-GCM under the seed's sealing key; a wrong code fails), checking that a longest order seals to exactly the service's 11095-character bound, rebuilding the same commitment, and registering with an intent signed from the view's fields;
- two record edits with consecutive nonces, each relayed after the previous is mined: `setTextWithSig`, then `clearRecordsWithSig`;
- redeeming by `approveWithSig` and `claimWithSig`, the token and registry node moving to the app's wallet;
- a commitment's limits to the second: at its `revealBefore` it reverts `CommitmentTooOld` and cannot be committed again while one a second younger registers, and it commits again a second later; an expired intent reverting `SignatureExpired`, and `ReverseRecordNotSupported`, `ResolverRequiredWhenDataSupplied` and `DurationTooShort`.

It uses the PR's own deployment fixture (`test/simplex/fixtures/namesV2.ts`), so it runs from inside a checkout of the PR:

```
git clone https://github.com/simplex-network/ens-contracts.git && cd ens-contracts
git fetch origin pull/36/head:pr36 && git checkout pr36
npx pnpm@9 install --frozen-lockfile --ignore-scripts
cp <simplex-chat>/plans/names-codes/e2e/*.ts test/simplex/
npx hardhat compile
NODE_OPTIONS="--disable-warning=ExperimentalWarning" npx vitest run test/simplex/StoreE2E.test.ts
```

Last run: PR #36 at `23892cc`, 6 tests passed.

## A local chain for the service

`deployLocal.ts` deploys the same stack to a running node, wired as the fixture wires it but with the PR deploy scripts' settings: `minCommitmentAge` 60 s, `maxCommitmentAge` 24 h, `maxLabelLength` 63, and the fixture's TLD `simplex`. The PR's scripts default to `testing` with the NFT gate on; `SIMPLEX_TLD` changes only the TLD here, and the gate stays off. The guardian (account 1) gives the registrar (account 2) an allowance. The script writes `local-chain.json` (addresses, the service key, the label cap and the commitment ages) and prints the service's `[names]` and `[chain]` config, whose `service_key` is account 2's well-known development key.

The node is anvil from Foundry (`foundryup`, or the `foundry_stable_linux_amd64.tar.gz` release), not `hardhat node`: Hardhat 3.1.4, which the PR pins, returned every revert we sent (`InvalidSignature` from `registerWithCredit`, and a plain call) as `-32603 Internal error` without its data, since its `_handleError` wraps any error that is not a `ProviderError`, so a client cannot tell `NameNotAvailable` from any other failure. Hardhat 3.19.0 fixes this: its `node/json-rpc/handler.js` `_handleError` extracts the revert data before wrapping the error, and answers a code 3 revert with the raw data, as geth and anvil do (checked in the published 3.19.0 package). Anvil's default accounts and chain id 31337 match the PR's `localhost` network.

```
anvil --chain-id 31337 --state local-chain.anvil        # keeps the deployment across restarts
npx hardhat run test/simplex/deployLocal.ts --network localhost
NODE_OPTIONS="--disable-warning=ExperimentalWarning" npx vitest run test/simplex/LocalChain.test.ts
```

`LocalChain.test.ts` checks the deployment as the service will use it. The service key signs raw transactions over HTTP. The test checks the wiring the config names, then commits a name with a record and one with a 64-character label. Before 60 s the register reverts `CommitmentTooNew`; after the wait it registers, spending exactly the list price. The long label reverts `LabelTooLong`. Each test can be rerun on the same deployment: the registered label and every secret are random, so each run makes new commitments, and the long label never registers. A run spends $20 of the $1000 allowance, so 49 runs succeed. The sponsor spends the allowance before it calls the controller, so the 50th, which empties it, then sees `InsufficientAllowance` instead of `LabelTooLong`, and the 51st fails the wiring check; deploy again then. The sponsor passes the controller's and base registrar's reverts through unchanged, so the test, like the service, decodes revert data against all three contracts' errors.

Last run: anvil 1.5.1, 2 tests passed.
