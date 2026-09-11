# Wallet keys

## Problem

The client needs keys of its own, outside the messaging protocol, at addresses
it can derive again after a restart, after a database restore, or on a new
device. Otherwise whatever an address holds is lost with the device.

The first use case is name ownership: a name bought in the app is owned by one
of these addresses. Buying is not implemented yet: The keys land first, so the key material
can be reviewed on its own, before the names protocol, the registrar and
signing.

## Design

One BIP-39 seed per device, one BIP-44 account per chat profile, one key per
name.

```
seed (BIP-39)
└── profile account i
    └── m/44'/60'/i'/0/k        one key per name; k = 0 is the profile's first
```

This is plain BIP-44: `account` and `address_index` are what those levels are
for, so the addresses match wallets people already use. The tests pin that
against the standard `abandon ... about` mnemonic:

```
m/44'/60'/0'/0/0   0x9858EfFD232B4033E47d90003D41EC34EcaEda94   MetaMask account 1
m/44'/60'/0'/0/1   0x6Fac4D18c912343BF86fa7049364Dd4E424Ab9C0   MetaMask account 2
m/44'/60'/1'/0/0   0x78839F6054d7ed13918bAe0473BA31b1Ca9D7265   Ledger Live account 2
```

So profile 0's names are MetaMask's account list in order, and each profile's
first name is the matching Ledger Live account. The mnemonic imports there as a
recovery phrase, a single secret as a private key.

**Why not one key per profile.** A name's owner is public, so an address that
owns several names links them: whoever knows one of them can read its owner and
find the rest. A key per name leaves no such link. It also keeps an export
narrow, as a name's secret is a leaf, with no chain code, so handing it over
hands over that name only.

**Why the account is a counter and not a hash of the profile.** A hashed index
would survive a restore in any profile order, and would leave no gap to disclose
a hidden profile. It would also change when a profile is renamed, and it would
move profile accounts off the list MetaMask and Ledger Live enumerate, which is
the point of the layout.

Stealth addresses will attach at purpose `5564'`, at the profile level. Only
purpose `44'` is used here.

The plan is one meta address per profile: a spend key and a viewing key, whose
public halves are published with the chat profile, opt-in. A sender derives a
fresh destination from it without a handshake, so one meta address serves any
number of incoming destinations, and those keys are not at a derivation path.
That is why it belongs at the profile level, while what a profile buys sits at
the address level.

## Commands

Internal API. The names commands will call these; users will not.

```
/_wallet                          this profile's first two name addresses, and
                                  the other profiles on the seed, by name
/_wallet create                   generate the seed. Refused if the device has one
/_wallet import <phrase>          store a seed. Refused if the device has one
/_wallet bind                     take the next free account
/_wallet bind <account>           claim one. Refused if another profile holds it
/_wallet export                   the seed mnemonic
/_wallet export <account> <name>  one derived secret, as 0x and 64 hex digits
/_wallet delete                   delete the seed, unbinding every profile
```

Creating the seed and claiming an account are separate. `create` and `import`
bind no profile, so no profile is put on an account it did not ask for. Neither
runs at startup or as a side effect of reading.

None of them is forwarded to a remote host: the recovery phrase must not leave
the device, and the raw command would be logged there.

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
column is empty while accounts already hold names, so a new profile would reuse
a recovered account's keys.

The table models several seeds, which a later change needs. One per device is
`single_seed` and a unique index on it, lifted later by a `DROP INDEX` and a
`DROP COLUMN`. It is a named index rather than an inline `UNIQUE` because SQLite
cannot drop a `UNIQUE` column or its automatic index, which would force a table
rebuild.

Migrations are sorted by name, and the runner requires the ones a database has
applied to be a prefix of that list. A later wallet migration has to sort after
`20260908_wallet_seeds`, or every existing database fails to start.

## What the phrase does not carry

A phrase carries entropy and nothing else. Two things are not in it, not on
chain, and not derivable.

**Which profile held which account.** A database backed up after the seed
carries the binding. One backed up before it comes back with the profiles and no
binding, and nothing records which profile was account 0. `/_wallet bind
<account>` is how the user states it, and the counter moves past what is claimed.

**Which accounts are taken.** `next_account_index` starts at 0 after an import,
so `/_wallet bind` with no account can hand out one that already owns names.
Only a scan of owned names can restore the mark.

## Hidden profiles

`/_wallet` names the other profiles on the seed and never numbers them, so a
hidden profile leaves no gap in a list of account indexes.

That hides it from the listing and nothing more. The seed is one per device, so
whoever unlocks any profile can export the phrase and derive every account,
including a hidden profile's. Hiding a profile does not make its names
pseudonymous against someone holding the device and one password. A key per
profile rather than per device is what would change that.

## Scope

Not here: buying a name, the names protocol, the registrar, signing, the
recovery scan, several seeds per device, stealth addresses.

`/_wallet` shows the first two addresses of the active profile's account, enough
to check the derivation against another wallet. It will show the names actually
held once those are recorded.
