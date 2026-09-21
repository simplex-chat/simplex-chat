# Wallet keys

## Problem

The client has no static keys of its own which could be used for blockchain use cases. It needs them at addresses it can derive again after a restart, after a database restore, or on a new device, or whatever those addresses hold is lost with the device.

The first consumer is [public namespaces](./2026-05-21-public-namespaces.md): a name is a record on a public blockchain saying which address owns it, and every name gets an account of its own, so one chat profile needs many accounts. This is not a names wallet only, though. An account is an ordinary Ethereum account, and [stealth transfers](https://github.com/simplex-chat/simplex-chat/pull/7519) will take accounts the same way. A device also holds several chat profiles, which are meant to stay isolated, and the one phrase the user writes down has to recover all of them.

Out of scope of this doc: buying a name, the names protocol, the registrar, signing, the scanning of the chain and the command that records what it finds, importing entropy for one keyring, more than one master, a backup of the counters shared between devices, and stealth keys. Buying is not implemented yet either, so the keys land first and can be reviewed on their own.

## Security objectives

1. Every key the device uses is derivable again from the master phrase alone.
2. Handing over one account key hands over that account and nothing else, whatever else the holder has.
3. Handing over one keyring phrase hands over that keyring and nothing else.
4. No extended public key links two keyrings, or two accounts within a keyring.
5. Key material is made and handed out only when asked for, never at startup and never as a side effect of reading.
6. A hidden profile owns nothing on chain, so nothing on chain is tied to it.

## Design

### Keyrings and accounts

A device has one piece of [BIP-39](https://github.com/bitcoin/bips/blob/master/bip-0039.mediawiki) entropy, the **master**, made as 24 words. Nothing signs with it. Every key comes from it in two steps.

First, [BIP-85](https://github.com/bitcoin/bips/blob/master/bip-0085.mediawiki) turns one node of a [BIP-32](https://github.com/bitcoin/bips/blob/master/bip-0032.mediawiki) tree into fresh BIP-39 entropy. The entropy at **keyring index** `r` is a **keyring**: its own 12 words, and every key under them. A keyring is what a chat profile is given, but it is not derived from the profile, and which profile has which keyring is a mapping in the database and nothing more. Second, [BIP-44](https://github.com/bitcoin/bips/blob/master/bip-0044.mediawiki) under a keyring gives each thing it owns on chain an **account index**, and the key there is the **account key**. Names are the first use: one name, one account, and that account's address is what the name record names as its owner.

```
master seed, 24 words                     the only thing to back up
└── keyring r  m/83696968'/39'/0'/12'/r'  BIP-85, its own 12-word mnemonic
    ├── m/44'/60'/n'/0/0                  accounts, n >= 0
    ├── m/5564'/60'/0'/0'/0               stealth spend               (later)
    └── m/5564'/60'/0'/1'/0               stealth view                (later)
```

### Derivation

BIP-85 derives entropy from a BIP-32 node by taking the bare 32-byte private key of that node and computing `HMAC-SHA512("bip-entropy-from-k", k)`, where `k` is that private key; the letter is the specification's, appears inside the string it fixes, and is not the keyring index. For 12 words the entropy is the leading 16 bytes. In the path, `39'` is BIP-85's BIP-39 application, `0'` is English, `12'` is the word count, `r'` is the keyring index, and every step is hardened.

Those 16 bytes are BIP-39 entropy, not a seed: they become 12 words, the words become a seed the BIP-39 way, and that seed is the root of a second BIP-32 tree, which is why both path shapes above start at `m`. Feeding the 16 bytes straight into BIP-32 would work inside the app and would silently break every claim here about another wallet.

An account index is BIP-44's own account level, the level [Ledger Live](https://github.com/LedgerHQ/ledger-live-common/blob/HEAD/docs/derivation.md) varies and also calls an account, numbering from one where its Account 1 is index 0. A keyring phrase given to another wallet reaches the same addresses, but that wallet finds account 0 and stops, because BIP-44 says to stop at the first account with no transaction history and an account that only owns a name never transacts. Above it the user has to enter the path.

The tests pin BIP-85 against the vectors in its own specification, and BIP-44 against the standard `abandon ... about` test mnemonic, which is 12 words, with an empty BIP-39 passphrase. Addresses are in [EIP-55](https://eips.ethereum.org/EIPS/eip-55) mixed case.

```
BIP-85, from the specification's own master key
m/83696968'/39'/0'/12'/0'   6250b68daf746d12a24d58b4787a714b
                            girl mad pet galaxy egg matter matrix prison refuse sense ordinary nose

BIP-44, from abandon ... about
account 0   m/44'/60'/0'/0/0   0x9858EfFD232B4033E47d90003D41EC34EcaEda94
account 1   m/44'/60'/1'/0/0   0x78839F6054d7ed13918bAe0473BA31b1Ca9D7265
```

### Why an account for each name

The alternative is one account holding a profile's names. A name's owner address is public, so that address would link them, and that is as true inside one profile as across two. An account for each name leaves no such link, and the same holds for anything else an account comes to own.

### Why the account level is hardened

The alternative is BIP-44's ordinary address level, `m/44'/60'/0'/0/n`, which is what MetaMask enumerates and is therefore the friendlier path. It is not hardened, and BIP-32 has a trap there: the extended public key above a key, together with that one key's private half, yields the parent private key and from it every sibling. Giving a keyring phrase to another wallet is how such an extended public key comes to exist, and giving away one account key is the other half. The two steps below an account are not hardened, so those halves do reach that account's own key, but nothing else is derived under an account and the account level itself is hardened, so they stop there; BIP-85 is hardened throughout, so they reach no other keyring either. That is objectives 2, 3 and 4, and it is worth the loss of MetaMask's default path.

### Why 24 words for the master and 12 for a keyring

Entropy cannot be added to a seed afterwards and what a seed is used for can grow, so the master is 24 words. A keyring is 12, because it is the phrase a user writes down when handing one keyring to another wallet, and 128 bits is the level secp256k1 works at, so nothing is lost at the point of use. The two lengths also tell the phrases apart at a glance: 24 words is everything on the device, 12 words is one keyring. For a keyring the count is part of the BIP-85 path, so it is fixed for good.

## Profiles and keyrings

A chat profile is bound to one keyring at a time, and a keyring to one profile. The binding is a db mapping, so it can be changed without any key moving, and nothing about the profile reaches the derivation. Profiles can bind no keyring: one is bound on first use, when a profile buys a name, so a user who never buys anything has a device that has never derived a key. Keyring indexes are handed out in order and never reused, because a keyring the device no longer tracks still owns whatever its accounts hold, and a keyring can sit unbound.

A hidden profile binds no keyring, so it cannot own a name. Two things would leak: the master derives every keyring under it, so unlocking any profile also derives a hidden profile's account keys; and a name is written into the profile's own database row and listed across the device, while a hidden profile is a filter on what is shown, not encryption. Closing either is work in the profiles and in the name record, not in the key layout. An incognito profile is meant to leave nothing behind, so it binds none either.

## Commands

An internal API, called by the names commands and by whatever else takes accounts later, rather than typed by users. Nothing here runs at startup or as a side effect of reading.

```
/_wallet                           whether the device has a master, and which
                                   keyring the active profile is bound to, and
                                   whether the master derives that keyring
/_wallet create new                generate the master, 24 words
/_wallet create mnemonic=<phrase>  take the master entropy from a phrase, 24 words
/_wallet bind keyring=<r>          bind the active profile to a keyring; without
                                   keyring=, the lowest unbound one, or a new
                                   one when there is none
/_wallet address keyring=<r> account=<n>
                                   one address, with the keyring and account
                                   index it came from; without account=, the
                                   next free account; without keyring=, the one
                                   the active profile is bound to
/_wallet export master             the master phrase
/_wallet export keyring            the bound keyring's phrase, 12 words
/_wallet export account <n>        one account key's secret, 0x and 64 hex
/_wallet delete                    delete the master entropy and its keyrings
```

`create` always names its source, so no key material is made by typing a prefix. An imported phrase must be 24 BIP-39 English words with a valid checksum, normalised for case and spacing. There is no BIP-39 passphrase, because it would be a second secret to back up and losing it would look exactly like losing the phrase, so a phrase used with one elsewhere lands on a different tree here and finds nothing.

`bind` without an argument takes the lowest unbound keyring and only makes a new one when there is none, which is what makes a recovery work without anyone naming anything. Naming one is required to rebind, which says plainly that the old keyring is being let go. After an import the master's counter is unknown rather than zero, because the phrase does not say how many keyrings it has been used for, so making a keyring is refused until a search sets it, while binding an existing one is still allowed. BIP-32 hardens an index by adding 2^31, so an index at or above 2^31 wraps into one that is not hardened, which is a silent loss of hardening rather than a collision; every index this API takes is refused there.

`address` reads the counter without moving it, so asking twice gives the same answer, and it works for a keyring the database has no row for, which is what a device that lost its database needs. One address at a time is enough: a caller sweeping the tree loops itself, and no form sweeps keyrings, because no single address answers for one, since what an account holds can go away.

`export` is a copy and not a handover: the device still derives what it exported and can still sign with it, so giving an account key away leaves two parties able to act as its owner until whatever it holds is transferred on chain. Signing is not in this change, and when it lands it is a command here that signs and returns a signature, not `export account` followed by signing elsewhere, which would make the narrow export the ordinary path. `delete` leaves accounts registered to their addresses, reachable only by the phrase.

```haskell
data WalletAddress = WalletAddress
  { keyringIndex :: Word32,
    accountIndex :: Word32,
    keyPath :: Text,
    address :: Text
  }

data WalletError
  = WENoMaster        -- the device has no master entropy
  | WEMasterExists    -- create, when it already has one
  | WEBadMnemonic     -- wrong word count, wrong word, or bad checksum
  | WEHiddenProfile   -- bind, on a profile the app hides
  | WEProfileBound    -- bind with no keyring=, on a profile that has one
  | WEProfileUnbound  -- export or address, on a profile that has none
  | WECounterUnknown  -- no counter to read yet, after an import
  | WEIndexTooLarge   -- at or above 2^31
```

## Recovery

Three cases, by how much of the database came back.

**Only the phrase is left.** A phrase carries entropy and nothing else. Which keyrings were used is not in it, so every counter starts out unknown and only a scan of the chain sets them; which profile was bound to which keyring is not in it either, and the order the user makes profiles in stands in for it. That scan is a nested walk over `address`, one call for each candidate: for each keyring in turn it asks for account 0, then 1, and so on, keeping on while accounts hold something and moving on after a run that holds nothing, and it ends after a run of keyrings that hold nothing at all. Asking what an address owns, deciding how long a run has to be, and recording the answers belong to the caller, because the wallet never sees an answer. Nothing outside this API writes these tables, so a command landing with the scan writes a row for each keyring found, unbound, and moves the master's counter past them.

What the scan finds keeps its own keyring index, nothing is attached to a profile, and the user is asked nothing. As the user makes profiles again and each buys its first name, `bind` takes the lowest unbound keyring, so profiles made in the order they were made before end up on the keyrings they had. A different order shows up as the wrong names, and `bind keyring=<r>` fixes it, moving no key and signing nothing, because ownership does not change, only which profile the app shows the accounts under. A keyring binds whole: splitting its accounts between two profiles would put an account under another keyring, which is an on-chain transfer and not a recovery step.

**The database is older than the master.** It has the profiles and the keyrings as of the backup and nothing written after it, so its counters are behind. A counter that is behind is worse than one that is unknown, because it looks usable, so such a database is treated as having unknown counters until the same scan fills the gap.

**The database is current.** It records which profile is bound to which keyring, so nothing is asked of the user.

## Schema

```sql
CREATE TABLE wallet_seeds (
  wallet_seed_id INTEGER PRIMARY KEY AUTOINCREMENT,
  entropy BLOB NOT NULL CHECK (length(entropy) IN (16, 32)),
  seed_kind TEXT NOT NULL CHECK (seed_kind IN ('master', 'keyring')),
  next_keyring_index INTEGER            -- master only; null means not known yet
);

CREATE TABLE wallet_keyrings (
  wallet_keyring_id INTEGER PRIMARY KEY AUTOINCREMENT,
  wallet_seed_id INTEGER NOT NULL REFERENCES wallet_seeds ON DELETE CASCADE,
  keyring_index INTEGER,                -- null when the entropy was imported
  user_id INTEGER REFERENCES users ON DELETE SET NULL,
  next_account_index INTEGER            -- from 0; null means not known yet
);

CREATE UNIQUE INDEX idx_wallet_seeds_master
  ON wallet_seeds(seed_kind) WHERE seed_kind = 'master';
CREATE UNIQUE INDEX idx_wallet_keyrings_index
  ON wallet_keyrings(wallet_seed_id, keyring_index);
CREATE UNIQUE INDEX idx_wallet_keyrings_imported
  ON wallet_keyrings(wallet_seed_id) WHERE keyring_index IS NULL;
CREATE UNIQUE INDEX idx_wallet_keyrings_user
  ON wallet_keyrings(user_id);
```

Only entropy that nothing can derive is stored: the master, always 32 bytes since it is made and imported as 24 words, and later a keyring imported on its own, 16 bytes, which is not implemented here. A derived keyring is never stored, because the master entropy and a keyring index derive it whenever a key is needed. So `wallet_keyrings` holds what derivation cannot produce: which keyring index exists, which stored entropy it belongs to, which profile is bound to it, and how far its account counter has run. A row with no `user_id` is a keyring no profile reaches, which is what a deleted chat profile leaves behind and what a scan writes.

`users` is not touched: the binding lives on the keyring row, and the unique index on `user_id` makes one keyring for each profile a rule of the database rather than of the caller, with null user ids distinct so any number can sit unbound. Deleting stored entropy takes its keyring rows, because a keyring index with no entropy behind it derives nothing, and the migration has no reverse step, because dropping `wallet_seeds` would destroy the only copy of the master entropy. `seed_kind` marks entropy the master does not cover, whose keyring row records a null `keyring_index`; such a row must never point at the master row, or `export keyring` would print the master phrase, and no constraint across two tables can say so in SQLite, so it is enforced where rows are written and there is a test for it.

## Threat model

- **Someone with the database file.** Gets everything, now and later: the stored entropy is the master phrase in another encoding, so an archive exported to move devices carries every key on the device. No export granularity helps against a file copy, and `delete` does not overwrite, so a deleted row survives in free pages and in the journal.
- **Someone with one account key.** Can act as that account's owner permanently, because an export is a copy and the device keeps deriving the same key. Cannot reach another account, the keyring above it, or another keyring.
- **Someone with one keyring phrase.** Gets every account under that keyring, including accounts taken later. Cannot reach another keyring or the master.
- **A wallet a keyring phrase is imported into.** Enumerating BIP-44 accounts computes account extended public keys, and some wallets send them to a vendor, which hands that vendor every account in the keyring at once. That is what an account for each name otherwise prevents.
- **Whoever answers the recovery scan.** Sees every address the phrase could hold a name on, in one burst, so it links every keyring on the device and recognises addresses that hold nothing yet, which is where future accounts will be. `address` derives for any index straight from the master, so a caller can enumerate hidden profiles' addresses too. This is the sharpest cost in the design.
- **A paired device.** Can run any of these commands, `export master` included, because they are not blocked from one. Only the command text is kept out of logs; the answer is not.

## Known limits

1. **The phrase alone does not restore a device.** Until the scan lands, an imported phrase gives a device its keys and no way to find what they own.
2. **The scan's shape is unsettled.** Going from an address to the names it owns is not something a registry answers on chain, so it needs the registrar or an indexer, and which one decides who sees the scan. How long a run of empty accounts, or of empty keyrings, ends a walk is a guess until it is written, and a user who knows better has to be able to send it further.
3. **The same phrase on two devices collides.** The counter lives in one database, so both make the same keyring and each believes it is free. Sharing it needs a backup both can read.
4. **A deleted chat profile leaves its keyring unbound, and the next profile to bind takes it.** That is what makes a recovery work without naming anything, and it is the wrong answer for a user who deleted a profile to be rid of it. Marking only the keyrings a scan found would fix it, at one column.
5. **A profile hidden after it was bound keeps its keyring.** The check is at bind time only.
6. **Keyring indexes are not dense.** A keyring can be made and never used, and a run of empty keyrings is how the scan stops, so one far above a gap can be missed.
7. **Gas and discovery pull against each other.** If an account ever pays for anything, whatever funds it links every account in the keyring on chain. If it never pays, no wallet finds it past account 0.
8. **Nothing records which layout a seed was used under.** A phrase used in another wallet may hold accounts at paths this doc does not describe.
9. **Purpose `5564'` is ours.** Reserved for stealth keys, taken from an [ERC-5564](https://eips.ethereum.org/EIPS/eip-5564) number rather than registered as a BIP-43 purpose, and nothing here derives at it.

## Files

- `src/Simplex/Chat/Wallet.hs`, derivation, pure.
- `src/Simplex/Chat/Store/Wallets.hs`, the two tables.
- `src/Simplex/Chat/Store/SQLite/Migrations/M20260908_wallet_seeds.hs` and the Postgres twin.
- `tests/WalletTests.hs`.
- BIP-85 lands in simplexmq as `Simplex.Messaging.Crypto.BIP85`, beside `BIP32` and `BIP39`, and adds no dependency, because the HMAC-SHA512 it needs is the one BIP-32 already uses.

## What is verified

**File:** `tests/WalletTests.hs`. Each of these is a test, not a claim.

1. **Vectors.** The BIP-85 vectors for application `39'` reproduce from the master key in its specification, and the two BIP-44 addresses above from `abandon ... about`.
2. **Interop.** A master phrase reaches an account address end to end, through BIP-85, through a real BIP-39 import of the 12 words it produced, and on to `m/44'/60'/1'/0/0`, so the BIP-39 step between the two trees cannot be skipped unnoticed.
3. **Isolation.** Two keyrings' first ten account addresses do not intersect, neither keyring's phrase derives the other's, and an account path hardens its account component.
4. **Refusals.** A second generate or import; a bad phrase; `bind` on a hidden profile, on a profile already bound, and making a keyring on an imported master; `export` with no argument, and `export keyring` for a profile bound to none; every index at or above 2^31.
5. **Binding and reads.** `bind` takes the lowest unbound keyring and rebinds only when named, profiles made in the same order after a recovery land on the keyrings they had, `address` returns the counter twice running without moving it and derives for a keyring with no row, and `export keyring` never returns the master phrase.
