# SimpleX names: registration through the sponsor service

Branch: `ab/names-registration-primitives-sponsored`, with simplexmq branch `ab/name-registration-primitives-sponsored` (PR #1941 into `names`).

Contracts: `SimplexController`, `SimplexSponsor` and `SimplexResolver` from ens-contracts `simplex` with PR #35, and branch `ab/sponsor-sign-duration` (PR #36), which adds the duration to `RegisterName`.

## Contents

1. Summary
2. User story
3. Scope
4. The flow
5. Code
6. Tests
7. Left to the plumbing PR

## 1. Summary

A user buys a `.simplex` name on the web, before the app supports wallets or name purchases, and without a browser wallet extension. The web UI makes a new wallet for the name, the service registers the name to that wallet and pays the gas, and the user keeps a redemption code from which the wallet's key is derived. Later the user redeems the code in the app.

This PR adds the functions this needs and tests that run the whole flow against a fake Ethereum node. It adds no plumbing: no worker, database, ini section or HTTP API.

## 2. User story

1. The user opens the names web UI, picks a name and pays.
2. The web UI generates a wallet for this one name.
3. The web UI shows a redemption code, `SN-XXXXX-XXXXX-XXXXX-XXXXX-XXXXX-XXXXX`. The wallet's key is derived from the code, so whoever has the code controls the name.
4. The service registers the name to the wallet's address. The user pays no gas and holds no ETH.
5. Later, once the app supports wallets, the user enters the code in the app, and the name is transferred to the app's wallet.

## 3. Scope

In this PR:

- name codes: generating, formatting and parsing the redemption code;
- the registration: its ABI encoding, the commitment, the `commit` and `registerWithCredit` calldata, and the owner's EIP-712 `RegisterName` intent and its verification;
- a JSON-RPC client that dry-runs, signs and sends the `commit` and `registerWithCredit` transactions;
- in simplexmq: EIP-712 hashing, signature serialization for the contract, and recovering the signer's address;
- test mocks for the web UI and for an Ethereum node, and tests.

Not in this PR:

- the web UI itself (simplexmq PR #1942), payments, and the service's worker, database, ini section and HTTP API;
- redeeming the code in the app. After PR #35 a name is transferred only with its recipient's consent: the code's wallet offers it (`approveWithSig`) and the app's wallet claims it (`claimWithSig`), both relayed by the service.

## 4. The flow

1. **Wallet and code (web UI).** The wallet's entropy is 128 random bits. Its key is the BIP-44 key at `m/44'/60'/0'/0/0` of the 12-word BIP-39 phrase with this entropy, so a BIP-44 wallet app can import it from that phrase. The code is `SN-` and 30 Crockford base32 characters: the entropy and the first 22 bits of its SHA-256.
2. **Commitment (web UI).** The web UI generates a 32-byte secret and builds the registration: label, TLD, owner (the wallet's address), duration, secret, resolver, and the `simplex.contact` and `simplex.channel` records. Records are `setText` calls on the resolver, `simplex.contact` then `simplex.channel`, links joined with `;`, and no call for a key without links. The commitment is `keccak256(abi.encode(Registration))`, as `SimplexController.makeCommitment` computes it.
3. **Commit (service).** The web UI sends only the commitment, so the name is not revealed. The service sends `commit(commitment)` to the controller.
4. **Intent (web UI).** The wallet signs `RegisterName(string label,address owner,uint256 duration,address resolver,bytes[] data,uint256 nonce,uint256 deadline)` under the `SimplexSponsor` EIP-712 domain. The nonce is the owner's count of intents the sponsor has used, so it is 0 for the new wallet. The web UI sends the registration, the nonce, the deadline and the signature.
5. **Verify (service).** The service checks that the secret is 32 bytes, the deadline has not passed, and the owner signed the intent.
6. **Register (service).** At least `minCommitmentAge` after the commit, the service sends `registerWithCredit(registration, nonce, deadline, signature)` to the sponsor. The sponsor checks the deadline, the nonce and the signature, spends the registrar's allowance, and the controller registers the name to the owner with its records.

Every transaction is first dry-run with `eth_estimateGas`; if it reverts, nothing is sent. The service signs EIP-1559 transactions with its registrar key: gas limit from the estimate, nonce from the registrar's pending transaction count, priority fee from `eth_maxPriorityFeePerGas`, maximum fee twice the latest base fee plus the priority fee.

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
- `tests/Bots/BadgeService/FakeEthNode.hs`, a JSON-RPC node that answers the five calls `sendCommit` and `sendRegister` make, records the transactions sent, and reverts the dry run on request.

## 6. Tests

`tests/Bots/BadgeService/NameRegistrationTests.hs`:

- the `commit` calldata of a registration's commitment, and the `registerWithCredit` calldata with and without records, against expected bytes;
- verification fails for an intent signed by another wallet, an intent past its deadline, and a secret that is not 32 bytes;
- nothing is sent when the dry run reverts;
- the whole flow: a new wallet, its code read back to the same wallet, the commit, the signed intent, its verification, and the registration, each transaction checked against the expected bytes sent to the controller and the sponsor.

`tests/BadgeTests.hs`: name code vectors, and a code with a wrong character does not parse.

simplexmq `tests/CoreTests/EthCryptoTests.hs`: an EIP-712 digest, the serialized signature, the recovered address, and every error in `recoverPublicKey`.

Expected values come from independent implementations:

- the commitment and calldata from viem, against the ABIs compiled from `ab/sponsor-sign-duration`, with the addresses and chain id of its test deployment;
- the simplexmq digest and signature from viem;
- name codes from a separate Python implementation.

Checked once against the contracts, outside CI:

- with the `ab/sponsor-sign-duration` test fixture, a call with the exact calldata the tests expect registered the name to the owner with both records, and the same call with one signature byte changed reverted with `InvalidSignature()`;
- on a local hardhat node with the `ab/sponsor-sign-duration` contracts, `sendCommit` and `sendRegister` registered the name over JSON-RPC.

Run:

```
cabal build -w ghc-9.6.3 test:simplex-chat-test
$(cabal list-bin -w ghc-9.6.3 test:simplex-chat-test) -m "name codes" -m "badge service name registration"
```

## 7. Left to the plumbing PR

- Call `verifyRegisterIntent` before `sendRegister`. The sponsor checks no signature, nonce or deadline when the owner is the registrar, and a secret that is not 32 bytes can change what the contract decodes.
- Parse the commit request's commitment as exactly 32 bytes. `commitCalldata` appends whatever it is given.
- Check the signed duration and the price against what was paid. The sponsor spends the registrar's allowance on the duration the owner signs. Price the order from the label's length in the oracle's price list, not with `rentPriceUSD(label, …)`, which sends the label to the node.
- Send `commit` only for a paid order. It costs the registrar gas, and the service cannot check a name the commitment hides.
- Register only the registration whose `makeCommitment` equals the commitment of the paid order the request names, once per order, and reject a commitment that is already in another order. Otherwise anyone can commit registrations of their own and have the sponsor spend the registrar's allowance on them.
- Accept only the default resolver, only a contact short link in `simplex.contact` and a channel short link in `simplex.channel`, as the app's `firstNameLink` reads them, bound their size, reject a `;` in a link, and cap the gas limit. The client sets the resolver and the links, the controller calls that resolver with the records, readers split a record into links at `;`, and the registrar pays for the gas the transaction uses, up to the estimate.
- Cap the priority fee and the maximum fee per gas. Both are set from the node's replies, and the registrar pays up to the maximum fee for each unit of gas used. Send `registerWithCredit` only while the current fees are within the caps, and replace it at market fees if it is not mined within a block or two: its calldata reveals the name, and once it has been pending for `minCommitmentAge` anyone can commit to the name and register it first.
- Accept only a label the web UI accepts: 6 to 63 letters a-z, digits 0-9 and single hyphens between them. The label is signed and committed as sent, so the service cannot normalize it, and resolution looks it up lower-cased.
- Take the TLD, `.simplex`, from the configuration, not from the request, and build the EIP-712 domain from the chain id and sponsor of the `ChainClient` passed to `sendRegister`.
- Parse the client's signature with v 27 or 28 and a low s, as the sponsor's ECDSA requires. libsecp256k1 also recovers a signer from a signature the sponsor rejects, such as one with a high s.
- Reject an intent whose deadline leaves too little time to commit, wait `minCommitmentAge` and register. The web UI sets the deadline from the browser's clock, an hour ahead, and a Bitcoin payment can take longer.
- Let the user sign again when a paid order's intent expired unused: the web UI rebuilds the wallet from the code, fetches the order's expired register request from the service, uses its label, links and secret only if its signature verifies under the rebuilt wallet's key for the web UI's own chain, sponsor, resolver, duration and TLD, shows the label and links for the user to confirm, and signs with a new deadline and the owner's sponsor nonce. The commitment covers neither, so it does not change.
- Send from one registrar key one transaction at a time, since the nonce is the registrar's pending transaction count.
- Store each signed transaction with its order before sending it. After an ambiguous error, send the same transaction again; replace one that is not mined only at its nonce, with higher fees within the caps. Before a new transaction for an order, check the commitment and the owner's sponsor nonce on chain, and the name only once its commitment has aged.
- Reconcile the sponsor's `RegistrarAllowanceSpent` and the controller's registrations with the orders, alert the guardian on a mismatch, and top up the allowance in small steps. Whoever holds a leaked registrar key can register any name they learn to a wallet of their own.
- Wait until the controller has the commitment, then `minCommitmentAge`, before registering, and register only while the commitment is more than a margin away from `maxCommitmentAge`; otherwise commit it again after it expires. Anyone can send the same commitment first, and then the registrar's own commit reverts.
- Call `sendRegister` only once the commitment has aged, as its dry run already sends the calldata to the node, and keep the label out of payment metadata and logs until the name is registered. The calldata reveals the name, and whoever sees it can commit to the name too. The same holds for any call with the label or its hash, such as `available`, `nameExpires` or `ownerOf`.
- Accept only an https or loopback RPC URL, bound the size of RPC responses, reject quantities that overflow their type, and check `eth_chainId` against the configured chain.
- Read the revert reason from `error.data` to tell a revert from a failed call, after checking that reth returns it there. A hardhat node does not when the revert comes from the controller through the sponsor.
- Keep the RPC URL out of logged errors, as it can contain an API key, and bound the node's error messages.
- Add `BadgeService.Names.Chain` and `BadgeService.Names.Registration` to the `simplex-badge-service` executable in `simplex-chat.cabal`; this PR adds them only to the test suite.
