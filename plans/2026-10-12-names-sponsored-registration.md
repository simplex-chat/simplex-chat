# SimpleX names: registration through the sponsor service

Branch: `ab/names-registration-primitives-sponsored`, with simplexmq branch `ab/name-registration-primitives-sponsored` (a PR into `names`).

Contracts: ens-contracts PR #35 (`SimplexController`, `SimplexSponsor`, `SimplexResolver`).

## Contents

1. Summary
2. User story
3. Scope
4. The flow
5. Code
6. Tests
7. Left to the plumbing PR

## 1. Summary

A user buys a `.simplex` name on the web, before the app supports wallets or name purchases, and without a browser wallet extension. The web UI makes a new wallet for the name, the service registers the name to that wallet and pays the gas, and the user keeps a redemption code that is the wallet. Later the user redeems the code in the app.

This PR adds the functions this needs and tests that run the whole flow against a fake Ethereum node. It adds no plumbing: no worker, database, ini section or HTTP API.

## 2. User story

1. The user opens the names web UI, picks a name and pays.
2. The web UI generates a wallet for this one name. Nothing is installed and no extension is needed.
3. The web UI shows a redemption code, `SN-XXXXX-XXXXX-XXXXX-XXXXX-XXXXX-XXXXX`. The code is the wallet: whoever holds it owns the name.
4. The store service registers the name to the wallet's address. The user pays no gas and holds no ETH.
5. Later, once the app supports wallets, the user enters the code in the app, and the app takes over the name.

## 3. Scope

In this PR:

- name codes: generating, formatting and parsing the redemption code;
- the registration: its ABI encoding, the commitment, the `commit` and `registerWithCredit` calldata, and the owner's EIP-712 `RegisterName` intent and its verification;
- a JSON-RPC client that dry-runs, signs and sends the `commit` and `registerWithCredit` transactions;
- in simplexmq: EIP-712 hashing, signature serialization for the contract, and recovering the signer's address;
- test mocks for the web UI and for an Ethereum node, and tests.

Not in this PR:

- the web UI itself, payments, and the service's worker, store, ini and HTTP API;
- redeeming the code in the app. After PR #35 a name moves only when its recipient takes it: the code's wallet offers it (`approveWithSig`) and the app's wallet claims it (`claimWithSig`), both relayed by the service.

## 4. The flow

1. **Wallet and code (web UI).** 128 bits of random entropy are the wallet. Its key is derived as from the 12-word BIP-39 phrase of the same entropy, at `m/44'/60'/0'/0/0`, so the wallet also works in any BIP-44 wallet app. The code is `SN-` and 30 Crockford base32 characters: the entropy and the first 22 bits of its SHA-256.
2. **Commitment (web UI).** The web UI chooses a 32-byte secret and builds the registration: label, TLD, owner (the wallet's address), duration, secret, resolver, and the `simplex.contact` and `simplex.channel` records. Records are `setText` calls on the resolver, links joined with `;`. The commitment is `keccak256(abi.encode(Registration))`, as `SimplexController.makeCommitment` computes it.
3. **Commit (service).** The client sends only the commitment, so the name is not revealed. The service sends `commit(commitment)` to the controller.
4. **Intent (web UI).** After `minCommitmentAge`, the wallet signs `RegisterName(string label,address owner,address resolver,bytes[] data,uint256 deadline)` under the `SimplexSponsor` EIP-712 domain. The client sends the registration, the deadline and the signature.
5. **Verify (service).** The service checks that the secret is 32 bytes, the deadline has not passed, and the owner signed the intent. A failed check costs no gas.
6. **Register (service).** The service sends `registerWithCredit(registration, deadline, signature)` to the sponsor. The sponsor checks the signature and spends the registrar's allowance, and the controller registers the name to the owner with its records.

Every transaction is first dry-run with `eth_estimateGas`. A revert sends nothing. The service signs EIP-1559 transactions with its registrar key: nonce from the pending block, priority fee from `eth_maxPriorityFeePerGas`, maximum fee twice the base fee plus the priority fee.

## 5. Code

simplex-chat:

- `src/Simplex/Chat/Badges/Code.hs`: `NameCode`, `randomNameCode`, `parseNameCode`, `formatNameCode`, `nameCodeEntropy`, next to the badge codes and sharing their alphabet and parsing rules.
- `apps/simplex-badge-service/src/BadgeService/Names/Registration.hs`: `Registration`, `RegisterIntent`, `sponsorDomain`, `makeCommitment`, `commitCalldata`, `registerWithCreditCalldata`, `registerIntentDigest`, `verifyRegisterIntent`.
- `apps/simplex-badge-service/src/BadgeService/Names/Chain.hs`: `ChainClient`, `ChainError`, `TxHash`, `sendCommit`, `sendRegister`.

simplexmq:

- `Simplex.Messaging.Eth.EIP712`: `hashTypedData` and `serializeSignature`.
- `recoverPublicKey` in `Simplex.Messaging.Crypto.Secp256k1` and `recoverAddress` in `Simplex.Messaging.Eth.Address`.

Test mocks:

- `tests/Bots/BadgeService/WebUIMock.hs`, the web UI's side: `newEphemeralWallet`, `redemptionCode`, `walletFromCode`, `signRegisterIntent`.
- `tests/Bots/BadgeService/FakeEthNode.hs`, a JSON-RPC node that answers the calls above, records the transactions sent, and reverts the dry run on request.

## 6. Tests

`tests/Bots/BadgeService/NameRegistrationTests.hs`:

- the commitment, the `commit` calldata, and the `registerWithCredit` calldata with and without records, against expected bytes;
- verification refuses an intent signed by another wallet, an intent past its deadline, and a secret that is not 32 bytes;
- a reverted dry run sends nothing;
- the whole flow: a new wallet, its code read back to the same wallet, the commit, the signed intent, its verification, the registration, and exactly the two expected transactions sent to the controller and the sponsor.

`tests/BadgeTests.hs`: name code vectors, and a code with a wrong character is refused.

simplexmq `tests/CoreTests/EthCryptoTests.hs`: the `RegisterName` digest, the serialized signature, the recovered address, and every refusal in `recoverPublicKey`.

Expected values come from independent implementations:

- calldata, commitments and signatures from viem, against the ABIs compiled from PR #35, with the addresses and chain id of the PR's test deployment;
- name codes from a separate Python implementation.

Checked once against the PR #35 contracts, outside CI:

- with the PR's own test fixture, the exact calldata the tests expect registered the name to the owner with both records, and a changed signature byte was refused with `InvalidSignature()`;
- on a local hardhat node, `sendCommit` and `sendRegister` registered the name over JSON-RPC.

Run:

```
cabal build -w ghc-9.6.3 test:simplex-chat-test
$(cabal list-bin -w ghc-9.6.3 test:simplex-chat-test) -m "name codes" -m "badge service name registration"
```

## 7. Left to the plumbing PR

- Call `verifyRegisterIntent` before `sendRegister`, so that a bad intent costs no gas and a secret that is not 32 bytes cannot change what the contract decodes.
- Check the duration and the price against what was paid. `RegisterName` does not sign the duration, and the registrar's allowance pays for it.
- Validate and lower-case the label. The contract hashes the label as given, and resolution lower-cases it.
- Send from one registrar key one transaction at a time, since the nonce comes from the pending block.
- Wait for the commit's receipt and `minCommitmentAge` before registering.
- Bound the size of RPC responses.
- Check that reth returns revert data in `error.data`. A hardhat node does not when the revert comes from the controller through the sponsor.
- Pin the simplexmq commit in `cabal.project` and `scripts/nix/sha256map.nix`.
