# Wallet keys

## Problem

The client has no static keys of its own which could be used for blockchain use cases. It needs them at addresses it can derive again after a restart, after a database restore, or on a new device, or whatever those addresses hold is lost with the device.

The first consumer is [public namespaces](./2026-05-21-public-namespaces.md): a name is a record on a public blockchain saying which address owns it, and every name gets an account of its own, so one chat profile needs many accounts. This is not a names wallet only, though. An account is an ordinary Ethereum account, and [stealth transfers](https://github.com/simplex-chat/simplex-chat/pull/7519) will take accounts the same way. A device also holds several chat profiles, which are meant to stay isolated, and the one phrase the user writes down has to recover all of them.

Out of scope of this doc: buying a name, the names protocol, the registrar, signing, the scanning of the chain and the command that records what it finds, importing an account key that the master does not derive, more than one master, and stealth keys. Buying is not implemented yet either, so the keys land first and can be reviewed on their own.

## Security objectives

1. Every key the device uses is derivable again from the master phrase alone.
2. Handing over one account key hands over that account and nothing else, whatever else the holder has.
3. No extended public key links two accounts.
4. Key material is made and handed out only when asked for, never at startup and never as a side effect of reading.
5. A hidden profile owns nothing on chain, so nothing on chain is tied to it.

There is no unit between the two: the master phrase is every account, and an account secret is one account. Nothing hands over "this profile's accounts", because nothing in the derivation knows about profiles.

## Design

### Accounts

A device has one piece of [BIP-39](https://github.com/bitcoin/bips/blob/master/bip-0039.mediawiki) entropy, the **master**, made as 24 words. Nothing signs with it. Every key is a [BIP-44](https://github.com/bitcoin/bips/blob/master/bip-0044.mediawiki) account under it, at a hardened account index, which is how Ledger Live lays out an Ethereum wallet.

Each thing the device owns on chain is given an **account index**, and the key there is the **account key**. Names are the first use: one name, one account, and that account's address is what the name record names as its owner. Which chat profile an account belongs to is a mapping in the database. No path mentions a profile, so nothing about a profile reaches the derivation and the two can be rearranged without a key moving.

```
master seed, 24 words     the only thing to back up
└── m/44'/60'/n'/0/0      account n, n >= 0
```

An account index is BIP-44's own account level, the level [Ledger Live](https://github.com/LedgerHQ/ledger-live-common/blob/HEAD/docs/derivation.md) varies and also calls an account, numbering from one where its Account 1 is index 0. So the master phrase given to another wallet reaches the same addresses and the two tools use the word for the same thing. That wallet finds account 0 and stops, because BIP-44 says to stop at the first account with no transaction history and an account that only owns a name never transacts; above it the user has to enter the path.

The tests pin the derivation against the standard `abandon ... about` test mnemonic, which is 12 words, with an empty BIP-39 passphrase. Addresses are in [EIP-55](https://eips.ethereum.org/EIPS/eip-55) mixed case.

```
account 0   m/44'/60'/0'/0/0   0x9858EfFD232B4033E47d90003D41EC34EcaEda94
account 1   m/44'/60'/1'/0/0   0x78839F6054d7ed13918bAe0473BA31b1Ca9D7265
```

### Why an account for each name

The alternative is one account holding several names. A name's owner address is public, so that address would link them, and that is as true inside one profile as across two. An account for each name leaves no such link, and the same holds for anything else an account comes to own.

### Why the account level is hardened

The alternative is BIP-44's ordinary address level, `m/44'/60'/0'/0/n`, which is what MetaMask enumerates and is therefore the friendlier path. It is not hardened, and [BIP-32](https://github.com/bitcoin/bips/blob/master/bip-0032.mediawiki) has a trap there: the extended public key above a key, together with that one key's private half, yields the parent private key and from it every sibling. Handing out one account key is one half, and any wallet that enumerates accounts produces the other. The two steps below an account are not hardened, so those halves do reach that account's own key, but nothing else is derived under an account and the account level itself is hardened, so they stop there and reach no other account. That is objectives 2 and 3, and it is worth the loss of MetaMask's default path.

### Why 24 words

Entropy cannot be added to a seed afterwards and what a seed is used for can grow, so the master is 24 words, 256 bits, which is a deliberate choice rather than a default.

## Profiles and accounts

An account is bound to at most one chat profile, and a profile to any number of accounts, because a profile can own any number of names. Only one of those names points at the profile as its SimpleX domain name, but which one is the names layer's record, not the wallet's: the wallet says only which profile an account belongs to.

Accounts are handed out in order and never reused, because an account the device no longer tracks still owns whatever it holds, and an account can sit unbound. Nothing is bound when a profile is made; an account is taken on first use, when a profile buys a name, so a user who never buys anything has a device that has never derived a key.

A hidden profile is bound no account, so it cannot own a name. Two things would leak: the master derives every account, so unlocking any profile also derives a hidden profile's account keys; and a name is written into the profile's own database row and listed across the device, while a hidden profile is a filter on what is shown, not encryption. Closing either is work in the profiles and in the name record, not in the key layout. Incognito is a property of a connection in this app rather than of a profile, so there is nothing at this level to refuse; an incognito connection has no profile of its own to bind an account to.

## Commands

An internal API, called by the names commands and by whatever else takes accounts later, rather than typed by users. Nothing here runs at startup or as a side effect of reading.

```
/_wallet                           whether the device has a master, and the
                                   accounts the active profile is bound to
/_wallet create new                generate the master, 24 words
/_wallet create mnemonic=<phrase>  take the master entropy from a phrase, 24 words
/_wallet bind account=<n>          bind an account to the active profile;
                                   without account=, the next free one
/_wallet address account=<n>       one address, with the account index it came
                                   from; without account=, the next free one
/_wallet export master             the master phrase
/_wallet export account <n>        one account key's secret, 0x and 64 hex
/_wallet delete                    delete the master entropy and its accounts
```

`create` always names its source, so no key material is made by typing a prefix. An imported phrase must be 24 BIP-39 English words with a valid checksum, normalised for case and spacing. There is no BIP-39 passphrase, because it would be a second secret to back up and losing it would look exactly like losing the phrase, so a phrase used with one elsewhere lands on a different tree here and finds nothing.

`bind` without an argument takes the next free account from a counter on the master, which is a high-water mark and not a count of what is held. Named with `account=<n>` it takes that one, which is how an account found by a scan is attached to the profile that should have it, and it is refused for an account another profile holds. After an import the counter is unknown rather than zero, because the phrase does not say how many accounts it has been used for, so taking a new one is refused until a scan sets it, while binding a known account is still allowed.

BIP-32 hardens an index by adding 2^31, so an index at or above 2^31 is already a hardened component and derives the same key as the index it wraps onto: account 2^31 is account 0. That is a collision, not a loss of hardening, and it would put one key under two account indexes. Every index this API takes is refused there, including one read from the counter, and the columns carry that bound so that whatever writes them later cannot slip past it. The counter's bound is one higher than an account's, because it holds the next index to hand out, and 2^31 there means every account that can be hardened has been handed out.

`address` reads the counter without moving it, so asking twice gives the same answer, and it works for an account the database has no row for, which is what a device that lost its database needs. One address at a time is enough: a caller scanning the tree loops itself.

`export account` is refused for an account another profile holds, because that key is not this profile's to hand out. An export is a copy and not a handover: the device still derives what it exported and can still sign with it, so giving an account key away leaves two parties able to act as its owner until whatever it holds is transferred on chain. Signing is not in this change, and when it lands it is a command here that signs and returns a signature, not `export account` followed by signing elsewhere, which would make the narrow export the ordinary path. `delete` leaves accounts registered to their addresses, reachable only by the phrase.

```haskell
data WalletAddress = WalletAddress {accountIndex :: Word32, keyPath :: Text, address :: Text}

data WalletError
  = WENoMaster        -- the device has no master entropy
  | WEMasterExists    -- create, when it already has one
  | WEBadMnemonic     -- wrong word count, wrong word, or bad checksum
  | WEHiddenProfile   -- bind, on a profile the app hides
  | WEAccountBound    -- bind or export account, on an account another profile holds
  | WECounterUnknown  -- no counter to read yet, after an import
  | WEIndexTooLarge   -- at or above 2^31
  | WEDerivation {derivationError :: String} -- BIP-32 or BIP-39 said no
```

## Recovery

Three cases, by how much of the database came back.

**Only the phrase is left.** A phrase carries entropy and nothing else. Which accounts were used is not in it, so the counter starts out unknown and only a scan of the chain sets it; which profile held which account is not in it either, and nothing recovers that. The scan is a walk over `address`, one call for each candidate: account 0, then 1, and so on, asking what each address owns and stopping after a run that owns nothing. Asking what an address owns, deciding how long that run has to be, and recording the answers belong to the caller, because the wallet never sees an answer. Nothing outside this API writes these tables, so a command landing with the scan writes a row for each account found, unbound, and moves the counter past them.

What the scan finds is unbound, and the user attaches each account to a profile with `bind account=<n>`. The names those accounts own are what identify them, which is what makes the question answerable at all: the user is choosing between names they recognise, not between numbers. Binding moves no key and signs nothing, because ownership does not change, only which profile the app shows the account under. Pointing a name at that profile's address is a separate signed edit of the name's record.

**The database is older than the master.** It has the accounts as of the backup and nothing written after it, so its counter is behind. A counter that is behind is worse than one that is unknown, because it looks usable and hands out an account the master has already used. Nothing here can tell a restored database from a current one, so clearing the counter belongs to whatever restores one, along with the scan that fills it in again.

**The database is current.** It records which profile holds which account, so nothing is asked of the user.

## Schema

```sql
CREATE TABLE wallet_seeds (
  wallet_seed_id INTEGER PRIMARY KEY AUTOINCREMENT,
  entropy BLOB NOT NULL CHECK (length(entropy) = 32),
  next_account_index INTEGER CHECK (next_account_index BETWEEN 0 AND 2147483648), -- null means not known yet
  single_seed INTEGER NOT NULL DEFAULT 1
);

CREATE TABLE wallet_accounts (
  wallet_account_id INTEGER PRIMARY KEY AUTOINCREMENT,
  wallet_seed_id INTEGER NOT NULL REFERENCES wallet_seeds ON DELETE CASCADE,
  account_index INTEGER CHECK (account_index BETWEEN 0 AND 2147483647), -- null when the key was imported
  user_id INTEGER REFERENCES users ON DELETE SET NULL
);

CREATE UNIQUE INDEX idx_wallet_seeds_single_seed ON wallet_seeds(single_seed);
CREATE UNIQUE INDEX idx_wallet_accounts_wallet_seed_id_account_index ON wallet_accounts(wallet_seed_id, account_index);
CREATE INDEX idx_wallet_accounts_user_id ON wallet_accounts(user_id);
```

Only entropy that nothing can derive is stored: the master, always 32 bytes, since it is made and imported as 24 words. An account key is never stored, because the master entropy and an account index derive it whenever one is needed. So `wallet_accounts` holds what derivation cannot produce, which account indexes the device knows about and which profile each belongs to. A row with no `user_id` is an account no profile holds, which is what a deleted chat profile leaves behind and what a scan writes.

`users` is not touched: the mapping lives on the account row, and the index on `user_id` is not unique, because a profile owns as many accounts as it owns names. One seed per device is `single_seed` and the unique index on it, which a later change lifts with a `DROP INDEX` and a `DROP COLUMN`; it is a named index rather than an inline `UNIQUE` because SQLite cannot drop one of those without rebuilding the table. Deleting the master takes its account rows, because an account index with no entropy behind it derives nothing. The migration has no reverse step, because reversing it would drop the only copy of the master entropy. What runs a reverse step is an older app installed over a newer database, which on mobile happens without asking and leaves one backup file that the next upgrade overwrites; with no reverse step that older app reports instead that the database is newer than it is, and changes nothing.

A null `account_index` marks an account whose key was imported rather than derived, which the master phrase does not recover and the schema must not suggest it does. Importing one is not implemented here; the column is nullable now so that a row written later reads correctly, rather than leaving an unmarked row to be guessed at.

## Threat model

- **Someone with the database file.** Gets everything, now and later: the stored entropy is the master phrase in another encoding, so an archive exported to move devices carries every key on the device. No export granularity helps against a file copy. On SQLite the connection sets `secure_delete`, so a deleted row's pages are zeroed; the journal and any copy already taken are not.
- **Someone with one account key.** Can act as that account's owner permanently, because an export is a copy and the device keeps deriving the same key. Cannot reach another account.
- **A wallet the master phrase is imported into.** Enumerating BIP-44 accounts computes account extended public keys, and some wallets send them to a vendor, which hands that vendor every account on the device at once, across every profile. That is what an account for each name otherwise prevents.
- **Whoever answers the recovery scan.** Sees every address the phrase could hold a name on, in one burst, so it links every account on the device, across profiles, and recognises addresses that hold nothing yet, which is where future accounts will be. `address` derives for any index straight from the master, so a caller can enumerate hidden profiles' addresses too. This is the sharpest cost in the design.
- **A paired device.** Can run any of these commands, because they are not blocked from one: `export master` reads the whole wallet, `create` on a device that has none plants a seed the pairing controls, and `delete` destroys the only copy. Blocking `ExecChatStoreSQL` while allowing `export master` is not a coherent line, and the wallet commands need their own decision rather than the catch-all.
- **Someone reading the logs.** The core logs no command and no answer, so a phrase typed into `create` reaches no log.
- **A page open in the user's browser.** The websocket server in `apps/simplex-chat/Server.hs` accepts any local connection, asks for no token and checks no `Origin`, and websockets are not bound by the same origin policy, so any page loaded while that server runs can send `export master` and read the answer. It also prints every command it receives, that phrase included. Both are properties of that server, which this change gives something worth taking, and closing them is work there rather than in the wallet.

## Known limits

1. **There is no per-profile handover.** The master phrase is every account on the device and an account secret is one account, with nothing in between, so a user who wants to hand one profile's names to another wallet hands over one key for each name.
2. **The phrase alone does not restore a device.** Until the scan lands, an imported phrase gives a device its keys and no way to find what they own.
3. **The scan's shape is unsettled.** Going from an address to the names it owns is not something a registry answers on chain, so it needs the registrar or an indexer, and which one decides who sees the scan. How long a run of empty accounts ends the walk is a guess until it is written, and a user who knows better has to be able to send it further.
4. **The same phrase on two devices collides.** The counter lives in one database, so both take the same account and each believes it is free. Sharing it needs a backup both can read.
5. **A profile hidden after it was bound keeps its accounts.** The check is at bind time only.
6. **Account indexes are not dense.** An account can be taken and never used, and a run of empty accounts is how the scan stops, so one far above a gap can be missed.
7. **Gas and discovery pull against each other.** If an account ever pays for anything, whatever funds it links accounts on chain. If it never pays, no wallet finds it past account 0.
8. **Nothing records which layout a seed was used under.** A phrase used in another wallet may hold accounts at paths this doc does not describe.
9. **A restored database hands out an account that is already used.** Its counter is behind what the master has reached, and nothing detects that, so a name can be bought with an account that already owns one until the scan resets the counter.
10. **Purpose `5564'` is ours.** Reserved for stealth keys, taken from an [ERC-5564](https://eips.ethereum.org/EIPS/eip-5564) number rather than registered as a BIP-43 purpose, and nothing here derives at it.

## Files

- `src/Simplex/Chat/Wallet.hs`, derivation, pure.
- `src/Simplex/Chat/Store/Wallets.hs`, the two tables.
- `src/Simplex/Chat/Store/SQLite/Migrations/M20260921_wallet_seeds.hs` and the Postgres twin.
- `tests/WalletTests.hs`.
- `tests/SchemaDump.hs` and `tests/PostgresSchemaDump.hs`, which selected what to test by taking every migration after the last one without a reverse step, and now take every migration from the first one that has a reverse step, applying any that has none.
- Derivation uses the `BIP32` and `BIP39` modules already in simplexmq and adds no dependency.

## What is verified

**File:** `tests/WalletTests.hs`. Each of these is a test, not a claim.

1. **Vectors.** The two addresses above reproduce from `abandon ... about`, as does account 0's secret, pinned to the value another wallet shows for it. A 24 word phrase imported through the command reaches a pinned address end to end, so a change of path fails here rather than shipping, and the account a command names is the account whose key comes back.
2. **Isolation.** Ten accounts' addresses are all different, and an account path hardens its account component.
3. **Refusals.** A second generate; a phrase that is not 24 valid words; `bind`, `delete` and `export master` on a device with no wallet; `bind` on a hidden profile, on an account another profile holds, and on an imported master whose counter is unknown; `export account` for an account another profile holds; every index at or above 2^31, on `address`, `bind` and `export account` alike.
4. **Binding and reads.** A profile binds several accounts and exports its own, an account bound by index moves the counter past it so the next one does not collide and never moves it back, `bind account=<n>` attaches a scanned one, an account a deleted profile leaves behind is taken by another profile, and `address` returns the counter twice running without moving it and derives for an account with no row.
5. **Encoding and persistence.** An account secret whose first byte is zero keeps its 64 hex digits; the wallet, its accounts, the counter and the phrase survive a restart; and deleting the wallet takes its accounts and starts the counter over.
