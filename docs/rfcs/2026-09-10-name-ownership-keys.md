# Keys that own names

## Problem

A name bought in the app has to be owned by an address, and the client has to be
able to derive that address again after a restart, after a database restore, or
on a new device. Without that, a bought name is lost the moment the device is.

Buying a name is not in this PR. The key that will own it is, so that the key
material can be reviewed and merged on its own, ahead of the names protocol, the
registrar and signing.

The wider design is `2026-08-18-in-app-name-purchase-mvp.md`, which lands with
the names work. This document covers only what ships here.

## Design

One BIP-39 seed per device, one BIP-44 account per chat profile, one key per name.

```
seed (BIP-39)
└── profile account i
    └── m/44'/60'/i'/0/k        one key per name; k = 0 is the profile's first
```

Nothing here is a custom layout: `account` and `address_index` are what BIP-44
has those levels for, so the addresses line up with wallets people already use.
The tests pin that against the standard `abandon ... about` mnemonic:

```
m/44'/60'/0'/0/0   0x9858EfFD232B4033E47d90003D41EC34EcaEda94   MetaMask account 1
m/44'/60'/0'/0/1   0x6Fac4D18c912343BF86fa7049364Dd4E424Ab9C0   MetaMask account 2
m/44'/60'/1'/0/0   0x78839F6054d7ed13918bAe0473BA31b1Ca9D7265   Ledger Live account 2
```

So profile 0's names are MetaMask's account list in order, and each profile's
first name is the matching Ledger Live account. Both exports are importable
there: the mnemonic as a recovery phrase, a single secret as a private key.

**Why not one key per profile.** Exporting it would hand over every name that
profile owns, and the resolver keeps one nonce per signer, so a shared key would
serialise every name's record edits behind one counter. An index per name avoids
both. A name's secret is a leaf, with no chain code, so exporting it hands over
that name and nothing else.

**Why the account level is not hashed from the profile.** An index derived from
the display name would survive a restore in any profile order, and would remove
the index gaps that disclose a hidden profile. It would also break when a profile
is renamed, and it would take profile accounts off the list MetaMask and Ledger
Live enumerate, which is the compatibility the layout is for. The account stays a
counter, and a profile that needs a specific one asks for it.

Purpose `5564'` is where stealth addresses will attach, at the profile level.
This PR uses purpose `44'` only, so the two do not meet.

## Commands

API only. Nothing here is user facing: the names commands will call these, and
the user-facing surface is `/name keys ...` in the wider design.

```
/_wallet                        the first two name addresses of this profile's
                                account, and the other profiles on the same seed,
                                by name
/_wallet create                 generate the seed. Refused if the device has one
/_wallet import <phrase>        store a seed from a phrase. Refused if the device has one
/_wallet bind                   take the next free account
/_wallet bind <account>         claim one account. Refused if another profile holds it
/_wallet export                 the seed mnemonic
/_wallet export <account> <name>  one derived secret, as 0x and 64 hex digits
/_wallet delete                 delete the seed, unbinding every profile
```

Creating the seed and claiming an account are separate commands. `create` and
`import` bind no profile, so no profile is ever put on an account it did not ask
for. Neither is done at startup or as a side effect of reading.

Wallet commands are not forwarded to a remote host: the recovery phrase must not
leave the device, and the raw command would be logged there.

## Schema

```sql
CREATE TABLE wallet_seeds (
  wallet_seed_id INTEGER PRIMARY KEY AUTOINCREMENT,
  seed BLOB NOT NULL,
  next_account_index INTEGER NOT NULL DEFAULT 0,
  single_seed INTEGER NOT NULL DEFAULT 1
);
ALTER TABLE users ADD COLUMN wallet_seed_id INTEGER REFERENCES wallet_seeds;
ALTER TABLE users ADD COLUMN wallet_account_index INTEGER;
```

`next_account_index` is a high-water mark, deliberately not
`MAX(users.wallet_account_index)`. After a restore from the phrase alone that
column is empty while accounts already hold names, so a new profile would
silently reuse a recovered account's keys.

The table models several seeds because the follow-up needs them. One per device
is `single_seed` plus a unique index on it, which multi-seed drops with a
`DROP INDEX` and a `DROP COLUMN`. It is a named index rather than an inline
`UNIQUE` for exactly that reason: SQLite cannot drop a `UNIQUE` column or its
automatic index, so the inline form would have forced a table rebuild.

The migration is `20260908_wallet_seeds`, not the prototype's `20260818`.
Migrations are sorted by name and the runner requires the ones a database has
applied to be a prefix of the list, so a name sorting before `20260822_forward_link`
would give every existing database a state error. The follow-up migrations need
renaming past `20260908` for the same reason.

## What the phrase does not carry

A phrase carries entropy and nothing else. Two things are not in it, not on
chain, and not derivable.

**Which profile held which account.** A chat database backed up after the seed
carries the binding, and restoring it is the whole story. A backup older than the
seed comes back with the profiles and no binding, and nothing says which profile
was account 0. `/_wallet bind <account>` is how the user says so, and the counter
moves past what is claimed by hand.

**Which accounts are already taken.** `next_account_index` starts at 0 after an
import, so `/_wallet bind` with no account can hand out one that already owns
names. Only a scan of owned names can restore the mark, and that lands with the
registrar.

## Hidden profiles

`/_wallet` names the other profiles on the seed and never numbers them, so a
hidden profile leaves no gap in a list of account indexes.

That hides its existence from the listing, and nothing more. The seed is one per
device, so whoever unlocks any profile can export the phrase and derive every
account, including a hidden profile's. A hidden profile's names are not
pseudonymous against someone who already holds the device and one password. This
is a consequence of one seed per device, and a key per profile rather than per
device is what would change it.

## Scope

Not here, and unchanged from the prototype: buying a name, the names protocol,
the registrar, signing, `wallet_name_keys`, the recovery scan, several seeds per
device, stealth addresses.

`/_wallet` shows the first two addresses of the active profile's account, which
is enough to check the derivation against another wallet. Once `wallet_name_keys`
exists it will show the names actually held instead.

## What is verified

Each of these is a test, not a claim.

- The three addresses and the private key above reproduce from the standard
  mnemonic, so a name bought here is reachable from any BIP-44 wallet.
- A secret whose first byte is zero keeps its 64 hex digits.
- Nothing is created until asked for, and a second create or import is refused.
- A second profile gets its own account, and its addresses do not intersect the
  first profile's.
- The key and the addresses survive a restart.
- A database backed up after the seed needs no import; one backed up before it is
  rebound by index and every address comes back; a seed imported before the
  restore is replaced by what the backup held.
- A profile binds once, and only to an account BIP-32 can harden.
- An account index at or above 2^31 is refused rather than folded onto a low one.
