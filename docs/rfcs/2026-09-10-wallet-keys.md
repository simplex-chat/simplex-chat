# Wallet keys

## Problem

The client has no static keys of its own for blockchain use cases. It requires keys at addresses it can derive again after a restart, after a database restore, or on a new device; otherwise whatever those addresses hold is lost with the device.

The first consumer is [public namespaces](./2026-05-21-public-namespaces.md): a name is a record on a public blockchain that contains the owner's address, and every name is owned by an account of its own, so one chat profile requires many accounts. The wallet is not specific to names. An account is an ordinary Ethereum account, and [stealth transfers](https://github.com/simplex-chat/simplex-chat/pull/7519) will use accounts in the same way. A device also contains several chat profiles, which are meant to stay isolated, and the one phrase the user writes down has to recover the keys of all of them.

Out of scope of this doc: buying a name, the names protocol, the registrar, signing, recovery from the phrase, importing an account key that the master does not derive, more than one master, and stealth keys. Buying is not implemented yet either, so key derivation is merged first and can be reviewed on its own.

## Security objectives

1. Every key the device uses is derivable again from the master phrase alone.
2. Giving away one account key gives away that account and nothing else, whatever else the recipient has.
3. No extended public key links two accounts.
4. Key material is generated only by an explicit command, never at startup, and keys are derived only by the command that uses them.

The master phrase is the only key material exported, and it derives every account. An export of one profile's accounts is impossible, because the profile is not an input to the derivation.

## Design

### Accounts

A device has one piece of [BIP-39](https://github.com/bitcoin/bips/blob/master/bip-0039.mediawiki) entropy, the **master**, generated as 24 words. Nothing signs with it. Every key is a [BIP-44](https://github.com/bitcoin/bips/blob/master/bip-0044.mediawiki) account under it, at a hardened account index, which is how Ledger Live lays out an Ethereum wallet.

Each thing the device owns on chain is assigned an **account index**, and the key at that index is the **account key**. Names are the first use: one name, one account, and that account's address is the owner address in the name record. Which chat profile an account belongs to is a mapping in the database. No path contains a profile, so no profile data is an input to the derivation, and the mapping can be changed without changing any key.

```
master seed               the only key material to back up
└── m/44'/60'/n'/0/0      account n, n >= 0
```

An account index is BIP-44's own account level, which [Ledger Live](https://github.com/LedgerHQ/ledger-live-common/blob/HEAD/docs/derivation.md) also varies and calls an account, so the master phrase imported into another wallet derives the same addresses. Other wallets reach an account above 0 only by entering the path, in a wallet that accepts one such as MEW, Rabby or Frame (known limit 4).

The tests pin the derivation against the standard `abandon ... about` test mnemonic, which is 12 words, with an empty BIP-39 passphrase. Addresses are in [EIP-55](https://eips.ethereum.org/EIPS/eip-55) mixed case.

```
account 0   m/44'/60'/0'/0/0   0x9858EfFD232B4033E47d90003D41EC34EcaEda94
account 1   m/44'/60'/1'/0/0   0x78839F6054d7ed13918bAe0473BA31b1Ca9D7265
```

### Why an account for each name

The alternative is one account owning several names. A name's owner address is public, so that address would link them, and that is as true inside one profile as across two. An account for each name creates no such link, and the same holds for anything else an account comes to own.

### Why the account level is hardened

The alternative is BIP-44's ordinary address level, `m/44'/60'/0'/0/n`, which is what MetaMask enumerates and is therefore the friendlier path. It is not hardened, and [BIP-32](https://github.com/bitcoin/bips/blob/master/bip-0032.mediawiki) has a known weakness there: the extended public key of a parent, together with one non-hardened child's private key, yields the parent private key and from it every sibling. An account key that is given away is one half, and any wallet that enumerates accounts produces the other. The two levels below an account are not hardened, so the two halves together reveal the extended private key at the account level, m/44'/60'/n'; nothing else is derived under an account by this app and the account level itself is hardened, so they reveal no other account's key, except under account 0 where other wallets derive (known limit 4). This satisfies objectives 2 and 3, and it is worth the loss of MetaMask's default path.

### Why 24 words

Entropy cannot be added to a seed afterwards and the uses of a seed can grow, so a generated master is 24 words, 256 bits.

## Profiles and accounts

An account belongs to at most one chat profile, and a profile has any number of accounts, because a profile can own any number of names. Only one of those names points at the profile as its SimpleX domain name, but which one is recorded by the names layer, not by the wallet: the wallet records only which profile an account belongs to.

Accounts are created in order and never reused, because an account the device no longer tracks still owns whatever it holds. An account remains without a profile when its profile is deleted. No account is created with a profile; an account is created on first use, when the user buys a name for a profile, so a device on which the user buys nothing has never derived an account key.

An account is created for a hidden profile as for any other profile. A hidden profile is a filter on what is shown, not encryption, and every account is derived from the master, so whoever can use any profile on the device can derive a hidden profile's account keys. Incognito is a property of a connection in this app rather than of a profile; the random profile of an incognito connection is not a chat profile, so it cannot hold an account.

## Commands

These commands are an internal API, called by the names commands and by any later feature that creates accounts, rather than typed by users. Nothing here runs at startup.

```
/_wallet <userId>                 whether the device has a master, the next
                                  account, and the profile's accounts
/_wallet create new               generate the master, 24 words
/_wallet create account <userId>  create the next account for the profile and
                                  return its index, path and address
/_wallet export master            the master phrase
/_wallet delete                   delete the master entropy and its accounts
```

A command that acts on a profile's accounts names the profile and is rejected when that profile is not the active one, so switching profiles during a flow cannot create an account for the wrong profile.

The BIP-39 passphrase is empty, because a passphrase would be a second secret to back up, and losing it would look exactly like losing the phrase. Another wallet derives the same keys from the exported phrase only with an empty passphrase.

`create account` takes the index from a counter on the master, which is a high-water mark and not a count of accounts. The counter is incremented by `UPDATE … RETURNING`, and the account row is inserted in the same transaction. On Postgres the update locks the counter row, so concurrent creations are serialised, as SQLite transactions are. Every account index in the database is below the counter, so the insert cannot conflict with an existing account.

BIP-32 marks an index as hardened by setting its top bit, so account 2^31 would derive account 0's key: a collision, not a loss of hardening. An account index is therefore a simplexmq type holding values below 2^31; the columns have CHECK constraints for the bound, and a row outside it is an error. The counter's bound is one higher, because it holds the next index; an increment past 2^31 violates its constraint and is reported as a database error. For the one key in 2^128 that BIP-32 declares invalid, simplexmq recomputes it as SLIP-0010 specifies, up to two times; a third invalid key is an internal error, since no input is expected to reach it.

Signing is not in this change; when it is added, it is a command here that returns a signature. `delete` leaves whatever the accounts own on chain, recoverable only from the phrase.

```haskell
data WalletAddress = WalletAddress {accountIndex :: AccountIndex, keyPath :: Text, address :: Address}
data WalletInfo = WalletInfo {accountIndexes :: [AccountIndex], nextAccountIndex :: Word32}
```

A device without a wallet is the store error `SEWalletNotFound`, and a second wallet is `SEDuplicateWallet`. A counter past 2^31 is a database error, and a third invalid key is an internal error.

## Recovery

Recovery from the phrase is planned separately. It is one core command, allowed only on a device without a wallet: the core imports the phrase, sends each account to a service in a separate request, records the accounts in use and sets the counter past them.

A database restored from a backup contains the counter as of the backup. The counter is behind if an account was created after the backup, and `create account` can then return an account that is already in use (known limit 5).

## Schema

```sql
CREATE TABLE wallet_seeds (
  wallet_seed_id INTEGER PRIMARY KEY AUTOINCREMENT,
  entropy BLOB NOT NULL CHECK (length(entropy) IN (16, 20, 24, 28, 32)),
  next_account_index INTEGER NOT NULL CHECK (next_account_index BETWEEN 0 AND 2147483648),
  single_seed INTEGER NOT NULL DEFAULT 1
) STRICT;

CREATE TABLE wallet_accounts (
  wallet_account_id INTEGER PRIMARY KEY AUTOINCREMENT,
  wallet_seed_id INTEGER NOT NULL REFERENCES wallet_seeds ON DELETE CASCADE,
  account_index INTEGER NOT NULL CHECK (account_index BETWEEN 0 AND 2147483647),
  user_id INTEGER REFERENCES users ON DELETE SET NULL
) STRICT;

CREATE UNIQUE INDEX idx_wallet_seeds_single_seed ON wallet_seeds(single_seed);
CREATE UNIQUE INDEX idx_wallet_accounts_wallet_seed_id_account_index ON wallet_accounts(wallet_seed_id, account_index);
CREATE INDEX idx_wallet_accounts_user_id ON wallet_accounts(user_id);
```

The master entropy, generated as 32 bytes, is the only key material stored. Neither the master key nor an account key is stored, because the entropy and an account index derive them whenever they are required. So `wallet_accounts` contains what derivation cannot produce: which account indexes are recorded on the device and which profile each belongs to. A row with no `user_id` is an account no profile holds, which is the result of deleting a chat profile.

`users` is not changed: the mapping is stored in the account row, and the index on `user_id` is not unique, because a profile can hold any number of accounts. One seed per device is enforced by `single_seed` and the unique index on it, which a later change removes with a `DROP INDEX` and a `DROP COLUMN`; it is a named index rather than an inline `UNIQUE` because SQLite cannot drop an inline constraint without rebuilding the table. Deleting the master deletes its account rows, because an account index without its entropy derives nothing. The down migration drops both tables, which deletes the master entropy; it runs only when the user confirms "Downgrade and open chat" in an older app.

## Threat model

- **Someone with the database file.** Gets everything, now and later: the stored entropy is the master phrase in another encoding, so an archive exported to move devices contains every key on the device. No export granularity protects against a file copy. On SQLite the connection sets `secure_delete`, so a deleted row's pages are zeroed; the journal and any copy already made are not.
- **A wallet the master phrase is imported into.** Enumerating BIP-44 accounts computes account extended public keys, and some wallets send them to a vendor, which gives that vendor every account on the device at once, across every profile. That is what an account for each name otherwise prevents.
- **A paired device.** `create new`, `export master` and `delete` are rejected from a paired device, as the archive and database encryption commands are, because they return, replace or delete key material. `create account` is allowed.
- **Someone reading the logs.** No wallet command contains key material. On Postgres the client inlines parameters into the statement text, so server-side statement logging records the inserted entropy.
- **A page open in the user's browser.** The websocket server in `apps/simplex-chat/Server.hs`, which runs only with `--chat-server-port` and listens on 127.0.0.1, accepts any local connection, requires no token and checks no `Origin`, and websockets are not bound by the same origin policy, so any page loaded while that server runs can send `export master` and read the response. This is a property of that server, which this change makes a more valuable target, and closing it requires changes to that server rather than to the wallet.

## Known limits

1. **Only the whole wallet can be moved.** The master phrase is the only export, and it derives every account on the device, so a wallet given one profile's names also receives every other profile's names.
2. **The phrase cannot be imported yet.** Until recovery is implemented, the phrase restores the keys only in another wallet.
3. **The same master on two devices collides.** The counter is stored in one database, so two devices with copies of that database create the same account, and each treats it as free.
4. **Account 0 is left to other wallets.** MetaMask, Ledger Live and Trezor Suite present `m/44'/60'/0'/0/0` first, so a phrase exported to one of them is used at account 0 there. A wallet therefore starts its counter at 1. In turn, BIP-44 discovery in those wallets stops at the empty account 0, so they reach this app's accounts only by path. If an account ever pays for anything, whatever funds it links accounts on chain.
5. **`create account` on a restored database can return an account that is already in use.** Its counter can be below the highest index used, and nothing detects that, so a name can be bought with an account that already owns one.

## Main files

- `src/Simplex/Chat/Wallet.hs`, derivation.
- `src/Simplex/Chat/Store/Wallets.hs`, the two tables.
- `src/Simplex/Chat/Store/SQLite/Migrations/M20261030_wallet_seeds.hs` and the corresponding Postgres migration.
- `tests/WalletTests.hs`.
- Derivation uses the `BIP32`, `BIP39`, `Secp256k1` and `Eth.Address` modules from simplexmq (simplex-chat/simplexmq#1843) and adds no dependency to this package.

## What is verified

**File:** `tests/WalletTests.hs`. Each of these is a test, not a claim.

1. **Vectors.** The two addresses above are derived from `abandon ... about`. The address of an account created through the command is the address derived from the exported phrase at the same index, so a change of path fails here rather than in a release.
2. **Isolation.** Ten accounts' addresses are all different, and the paths of accounts 0 and 7 have a hardened account component.
3. **Rejections.** A second wallet; `create account`, `delete` and `export master` on a device with no wallet.
4. **Accounts.** Each `create account` returns the next index; each profile's accounts are listed apart; a deleted profile's account index is not reused.
5. **Persistence.** The wallet, its accounts, the counter and the phrase persist across a restart; after the wallet is deleted, a new wallet starts its counter at 1.
