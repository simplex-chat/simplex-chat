# Wallet keys

## Problem

The client has no static keys of its own for blockchain use cases. It requires keys at addresses it can derive again after a restart, after a database restore, or on a new device; otherwise whatever those addresses hold is lost with the device.

The first consumer is [public namespaces](./2026-05-21-public-namespaces.md): a name is a record on a public blockchain that contains the owner's address, and every name is owned by an account of its own, so one chat profile requires many accounts. The wallet is not specific to names. An account is an ordinary Ethereum account, and [stealth transfers](https://github.com/simplex-chat/simplex-chat/pull/7519) will use accounts in the same way. A device also contains several chat profiles, which are meant to stay isolated, and the one phrase the user writes down has to recover the keys of all of them.

Out of scope of this doc: buying a name, the names protocol, the registrar, signing, the chain scan and the command that records its results, importing an account key that the master does not derive, more than one master, and stealth keys. Buying is not implemented yet either, so key derivation is merged first and can be reviewed on its own.

## Security objectives

1. Every key the device uses is derivable again from the master phrase alone.
2. Giving away one account key gives away that account and nothing else, whatever else the recipient has.
3. No extended public key links two accounts.
4. Key material is generated or imported only by an explicit command, never at startup, and keys are derived only by the command that uses them.

The master phrase derives every account and an account secret controls one account; no export covers anything in between, such as "this profile's accounts", because the profile is not an input to the derivation.

## Design

### Accounts

A device has one piece of [BIP-39](https://github.com/bitcoin/bips/blob/master/bip-0039.mediawiki) entropy, the **master**, generated as 24 words. Nothing signs with it. Every key is a [BIP-44](https://github.com/bitcoin/bips/blob/master/bip-0044.mediawiki) account under it, at a hardened account index, which is how Ledger Live lays out an Ethereum wallet.

Each thing the device owns on chain is assigned an **account index**, and the key at that index is the **account key**. Names are the first use: one name, one account, and that account's address is the owner address in the name record. Which chat profile an account belongs to is a mapping in the database. No path contains a profile, so no profile data is an input to the derivation, and the mapping can be changed without changing any key.

```
master seed               the only key material to back up
└── m/44'/60'/n'/0/0      account n, n >= 0
```

An account index is BIP-44's own account level, which [Ledger Live](https://github.com/LedgerHQ/ledger-live-common/blob/HEAD/docs/derivation.md) also varies and calls an account, so the master phrase imported into another wallet derives the same addresses. Other wallets reach an account above 0 only by entering the path, in a wallet that accepts one such as MEW, Rabby or Frame, or with the exported account key (known limit 6).

The tests pin the derivation against the standard `abandon ... about` test mnemonic, which is 12 words, with an empty BIP-39 passphrase. Addresses are in [EIP-55](https://eips.ethereum.org/EIPS/eip-55) mixed case.

```
account 0   m/44'/60'/0'/0/0   0x9858EfFD232B4033E47d90003D41EC34EcaEda94
account 1   m/44'/60'/1'/0/0   0x78839F6054d7ed13918bAe0473BA31b1Ca9D7265
```

### Why an account for each name

The alternative is one account owning several names. A name's owner address is public, so that address would link them, and that is as true inside one profile as across two. An account for each name creates no such link, and the same holds for anything else an account comes to own.

### Why the account level is hardened

The alternative is BIP-44's ordinary address level, `m/44'/60'/0'/0/n`, which is what MetaMask enumerates and is therefore the friendlier path. It is not hardened, and [BIP-32](https://github.com/bitcoin/bips/blob/master/bip-0032.mediawiki) has a known weakness there: the extended public key of a parent, together with one non-hardened child's private key, yields the parent private key and from it every sibling. An exported account key is one half, and any wallet that enumerates accounts produces the other. The two levels below an account are not hardened, so the two halves together reveal the extended private key at the account level, m/44'/60'/n'; nothing else is derived under an account by this app and the account level itself is hardened, so they reveal no other account's key, except under account 0 where other wallets derive (known limit 6). This satisfies objectives 2 and 3, and it is worth the loss of MetaMask's default path.

### Why 24 words

Entropy cannot be added to a seed afterwards and the uses of a seed can grow, so a generated master is 24 words, 256 bits. An imported phrase is accepted at any BIP-39 length, 12 to 24 words, because its strength was decided by the wallet that generated it and other wallets default to 12.

## Profiles and accounts

An account is bound to at most one chat profile, and a profile to any number of accounts, because a profile can own any number of names. Only one of those names points at the profile as its SimpleX domain name, but which one is recorded by the names layer, not by the wallet: the wallet records only which profile an account belongs to.

Accounts are allocated in order and never reused, because an account the device no longer tracks still owns whatever it holds. An account can remain unbound. Nothing is bound when a profile is created; an account is bound on first use, when the user buys a name for a profile, so a device on which the user neither buys anything nor requests an address has never derived an account key.

A hidden profile binds accounts like any other profile. Hiding a profile filters what is shown and encrypts nothing, and the master derives every account, so whoever can use any profile on the device can derive a hidden profile's account keys. Incognito is a property of a connection in this app rather than of a profile; the random profile of an incognito connection is not a chat profile, so it cannot hold an account.

## Commands

These commands are an internal API, called by the names commands and by any later feature that binds accounts, rather than typed by users. Nothing here runs at startup.

```
/_wallet <userId>                     whether the device has a master, the next
                                      account, and the accounts bound to the profile
/_wallet create new                   generate the master, 24 words
/_wallet create mnemonic=<phrase>     import the master entropy from a phrase, 12 to 24 words
/_wallet bind <userId> account=<n>    bind an account to the profile and return its
                                      address; without account=, the next free one
/_wallet address account=<n>          one address, with the account index it is
                                      derived at; without account=, the next free one
/_wallet export master                the master phrase
/_wallet export account <userId> <n>  the secret of an account the profile holds,
                                      0x and 64 hex digits
/_wallet delete                       delete the master entropy and its accounts
```

A command that acts on a profile's accounts names the profile and is rejected when that profile is not the active one, so switching profiles during a flow cannot bind an account to the wrong profile.

`create` always names its source, so no key material is generated by typing a prefix. An imported phrase must be 12, 15, 18, 21 or 24 BIP-39 English words with a valid checksum, normalised for case and spacing. There is no BIP-39 passphrase, because it would be a second secret to back up and losing it would look exactly like losing the phrase, so a phrase used with a passphrase elsewhere derives different keys here.

`bind` without `account=` binds the account at a counter on the master, which is a high-water mark and not a count of bound accounts, and returns the bound account's index, path and address. With `account=<n>` it binds that account, which is how an account found by a scan is attached to the profile it belongs to, and it is rejected for an account another profile holds. After an import the counter is unknown rather than zero, because the phrase does not encode how many accounts it has been used for, so binding the next account is rejected until a scan sets the counter, while binding a known account is still allowed.

BIP-32 marks an index as hardened by setting its top bit, so account 2^31 would derive account 0's key: a collision, not a loss of hardening. An account index is therefore a simplexmq type holding values below 2^31; the parser rejects larger values as a bad command, the columns have CHECK constraints for the bound, and a row outside it is an error. The counter's bound is one higher, because it holds the next index to bind; at 2^31 it has passed every index, which `bind` without an index reports. For the one key in 2^128 that BIP-32 declares invalid, simplexmq recomputes it as SLIP-0010 specifies, up to three times; a third failure is an internal error, since no input is expected to reach it.

`address` reads the counter without changing it, so two calls return the same address, and it derives an address for an account the database has no row for, which a device that lost its database requires. One address per call is sufficient: a caller that scans the tree calls it in a loop.

`export account` is rejected unless the profile holds the account, so every exported key belongs to a bound account, which `bind` without an index does not return again, except on a database restored from a backup (known limit 8). An export is a copy, not a transfer: the device still derives the key, so two parties can act as the owner until whatever the account holds is transferred on chain. Signing is not in this change; when it is added it is a command here that returns a signature, not `export account` followed by signing elsewhere. `delete` leaves whatever the accounts own on chain, recoverable only from the phrase.

```haskell
data WalletAddress = WalletAddress {accountIndex :: AccountIndex, keyPath :: Text, address :: Address}
data WalletInfo = WalletInfo {accountIndexes :: [AccountIndex], nextAccountIndex :: Maybe Word32} -- Nothing: unknown after an import

data WalletError
  = WEAccountBound    -- bind, on an account another profile holds
  | WEAccountNotHeld  -- export account, on an account the profile does not hold
  | WECounterUnknown  -- the counter is not set yet, after an import
  | WEAccountsExhausted -- bind without an index, when the counter has passed every index
```

## Recovery

Three cases, by how much of the database is restored.

**Only the phrase is left.** A phrase encodes the entropy and nothing else. Which accounts were used is not encoded in it, so the counter starts out unknown and only a scan of the chain sets it; which profile held which account is not encoded in it either, and nothing recovers that. The scan is a sequence of `address` calls, one for each candidate: account 0, then 1, and so on, querying what each address owns and stopping after a run of addresses that own nothing. Querying what an address owns and choosing the length of that run are the caller's responsibility, because `address` returns only an address. Nothing outside this API adds rows to these tables, so a command added together with the scan writes a row for each account found, unbound, and moves the counter past every account row, including accounts bound by index that the scan does not find.

What the scan finds is unbound, and the user attaches each account to a profile with `bind account=<n>`. The names those accounts own identify them, which is what makes the choice possible at all: the user chooses between names they recognise, not between numbers. Binding changes no key and signs nothing, because ownership does not change, only the profile under which the app shows the account. Pointing a name at that profile's SimpleX address is a separate signed edit of the name's record.

**The database is restored from a backup.** It contains the accounts as of the backup and nothing written after it, so its counter is behind if an account was bound after the backup. A counter that is behind is worse than one that is unknown, because it appears valid, so `bind` can return an account that is already in use. Nothing here distinguishes a restored database from a current one, so clearing the counter is the responsibility of whatever restores a database, along with the scan that sets it again.

**The database is current.** It records which profile holds which account, so the user is asked nothing.

## Schema

```sql
CREATE TABLE wallet_seeds (
  wallet_seed_id INTEGER PRIMARY KEY AUTOINCREMENT,
  entropy BLOB NOT NULL CHECK (length(entropy) IN (16, 20, 24, 28, 32)),
  next_account_index INTEGER CHECK (next_account_index BETWEEN 0 AND 2147483648), -- null means not known yet
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

The master entropy, 32 bytes when generated and 16 to 32 bytes when imported, is the only key material stored. `create` derives the first account before storing the entropy, so stored entropy is known to derive keys. Neither the master key nor an account key is stored, because the entropy and an account index derive them whenever they are required. So `wallet_accounts` contains what derivation cannot produce: which account indexes are recorded on the device and which profile each belongs to. A row with no `user_id` is an account no profile holds, which is the result of deleting a chat profile and what a scan writes.

`users` is not changed: the mapping is stored in the account row, and the index on `user_id` is not unique, because a profile can hold any number of accounts. One seed per device is enforced by `single_seed` and the unique index on it, which a later change removes with a `DROP INDEX` and a `DROP COLUMN`; it is a named index rather than an inline `UNIQUE` because SQLite cannot drop an inline constraint without rebuilding the table. Deleting the master deletes its account rows, because an account index without its entropy derives nothing. The down migration drops both tables, which deletes the master entropy; it runs only when the user confirms "Downgrade and open chat" in an older app.

## Threat model

- **Someone with the database file.** Gets everything, now and later: the stored entropy is the master phrase in another encoding, so an archive exported to move devices contains every key on the device. No export granularity protects against a file copy. On SQLite the connection sets `secure_delete`, so a deleted row's pages are zeroed; the journal and any copy already made are not.
- **A wallet the master phrase is imported into.** Enumerating BIP-44 accounts computes account extended public keys, and some wallets send them to a vendor, which gives that vendor every account on the device at once, across every profile. That is what an account for each name otherwise prevents.
- **Whoever answers the recovery scan.** Receives every address the scan derives from the phrase, in one sequence of requests, so it can link every account on the device, across profiles, and recognise addresses that own nothing yet, which is where future accounts will be. This is the largest privacy cost of the design.
- **A paired device.** Wallet commands are allowed from a paired device like other chat commands: `export master` returns the whole wallet, `create mnemonic=` on a device that has no seed imports a phrase that the paired device sends, and `delete` deletes the master entropy, which may have no other copy.
- **Someone reading the logs.** The core logs no command and no response, so a phrase passed to `create` is not written to any log. On Postgres the client inlines parameters into the statement text, so server-side statement logging records the inserted entropy.
- **A page open in the user's browser.** The websocket server in `apps/simplex-chat/Server.hs`, which runs only with `--chat-server-port` and listens on 127.0.0.1, accepts any local connection, requires no token and checks no `Origin`, and websockets are not bound by the same origin policy, so any page loaded while that server runs can send `export master` and read the response. It also prints every command it receives, that phrase included. Both are properties of that server, which this change makes a more valuable target, and closing them requires changes to that server rather than to the wallet.

## Known limits

1. **There is no per-profile transfer.** The master phrase derives every account on the device and an account secret controls one account, with nothing in between, so a user who wants to move one profile's names to another wallet has to export one key for each name.
2. **The phrase alone does not restore a device.** Until the scan is implemented, an imported phrase gives a device its keys and no way to find what they own.
3. **The scan's design is not decided.** Finding the names an address owns is not a query a registry answers on chain, so it requires the registrar or an indexer, and that choice determines which party observes the scan. The length of the run of empty accounts that ends the scan is not yet decided, and the user must be able to extend the scan.
4. **The same phrase on two devices collides.** The counter is stored in one database, so both devices bind the same account and each treats it as free. Sharing the counter requires a backup both devices can read.
5. **Account indexes are not dense.** An account can be bound and never used, and a run of empty accounts ends the scan, so an account after a gap can be missed.
6. **Account 0 is left to other wallets.** MetaMask, Ledger Live and Trezor Suite present `m/44'/60'/0'/0/0` first and hand a browser the extended public key of its address level, so with a phrase used in one of them the export of account 0 exposes that wallet's accounts. A generated wallet therefore starts its counter at 1, and account 0 is bound or exported only when asked for with `account=0`. In turn, BIP-44 discovery in those wallets stops at the empty account 0, so they reach this app's accounts only by path or exported key. If an account ever pays for anything, whatever funds it links accounts on chain.
7. **Nothing records which layout a seed was used with.** Another wallet may have derived accounts from the phrase at paths this doc does not describe.
8. **`bind` on a restored database can return an account that is already in use.** Its counter can be below the highest index used, and nothing detects that, so a name can be bought with an account that already owns one until the scan resets the counter.

## Main files

- `src/Simplex/Chat/Wallet.hs`, derivation.
- `src/Simplex/Chat/Store/Wallets.hs`, the two tables.
- `src/Simplex/Chat/Store/SQLite/Migrations/M20260924_wallet_seeds.hs` and the corresponding Postgres migration.
- `tests/WalletTests.hs`.
- Derivation uses the `BIP32`, `BIP39`, `Secp256k1` and `Eth.Address` modules from simplexmq (simplex-chat/simplexmq#1843) and adds no dependency to this package.

## What is verified

**File:** `tests/WalletTests.hs`. Each of these is a test, not a claim.

1. **Vectors.** The two addresses above are derived from `abandon ... about`, as is account 0's secret, pinned to the value another wallet shows for it. A 24 word phrase imported through the command derives a pinned address end to end, so a change of path fails here rather than in a release, and the account a command names is the account whose key is returned.
2. **Isolation.** Ten accounts' addresses are all different, and the paths of accounts 0 and 7 have a hardened account component.
3. **Rejections.** A second generate; a phrase with a bad checksum; `bind`, `delete` and `export master` on a device with no wallet; `bind` on an account another profile holds, and on an imported master whose counter is unknown; `export account` for an account the profile does not hold; `bind` without an index once the counter has passed 2^31 - 1; an index of 2^31 or more is rejected as a bad command on `address`, `bind` and `export account` alike.
4. **Binding and reads.** Several accounts are bound to one profile, each `bind` returns the account it bound, and that profile's accounts can be exported; binding an account by index moves the counter past it so the next one does not collide, and never moves it back; `bind account=<n>` binds an account after an import; an account left unbound by deleting its profile is bound to another profile; and two consecutive `address` calls return the same address without changing the counter, and derive an address for an account with no row.
5. **Encoding and persistence.** An account secret whose first byte is zero is rendered with 64 hex digits; the wallet, its accounts, the counter and the phrase persist across a restart; and deleting the wallet deletes its accounts and resets the counter.
