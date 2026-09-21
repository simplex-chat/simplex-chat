# Wallet keys

## Problem

The client needs keys of its own, outside the messaging protocol, at addresses
it can derive again after a restart, after a database restore, or on a new
device. Otherwise whatever an address holds is lost with the device.

The first use case is name ownership: a name bought in the app is owned by one
of these addresses. Buying is not implemented yet. The keys land first, so the
key material can be reviewed on its own, before the names protocol, the
registrar and signing.

A device holds several chat profiles, and they are meant to stay apart. The
layout below gives each profile a seed of its own that the one phrase still
recovers.

## Design

One BIP-39 master seed per device. It signs nothing. Every key sits under a
per-profile seed that BIP-85 derives from it.

```
master seed, 24 words                     the only thing to back up
└── profile p  m/83696968'/39'/0'/24'/p'  BIP-85, its own 24-word mnemonic
    ├── m/44'/60'/0'/0/0                  the profile's own address   (later)
    ├── m/44'/60'/n'/0/0                  names, n >= 1
    ├── m/5564'/60'/0'/0'/0               stealth spend               (later)
    └── m/5564'/60'/0'/1'/0               stealth view                (later)
```

BIP-85 derives entropy from a BIP-32 node: take the node's private key `k`, the
letter its specification uses, and read the leading bytes of
`HMAC-SHA512("bip-entropy-from-k", k)`. Application `39'` reads those bytes as
BIP-39 entropy, with the language, the word count and the index in the path, so
`0'` is English, `24'` is the word count and `p'` is the profile. Every index is
hardened, so the derivation never needs the public-key half of BIP-32, which is
the half simplexmq deliberately does not implement.

BIP-85 lands in simplexmq as `Simplex.Messaging.Crypto.BIP85`, on top of the
BIP-32 and BIP-39 modules already there. It needs no new dependency: the
HMAC-SHA512 it uses is the one BIP-32 already uses.

Below the profile seed this is plain BIP-44, with a name at the account level.
An exported profile mnemonic therefore reaches the same addresses in another
wallet: that profile's names are Ledger Live's account list from its second
entry on, which Ledger Live, Rabby and others enumerate. The tests pin BIP-85
against the vectors in its own specification, and BIP-44 against the standard
`abandon ... about` mnemonic:

```
m/44'/60'/0'/0/0   0x9858EfFD232B4033E47d90003D41EC34EcaEda94   Ledger Live account 1
m/44'/60'/1'/0/0   0x78839F6054d7ed13918bAe0473BA31b1Ca9D7265   Ledger Live account 2
```

**Why a seed per profile.** What a user can hand over is otherwise all or
nothing: the device phrase hands over every profile, a name's raw secret hands
over one name, and there is nothing in between. A profile seed is the thing in
between, and it is a mnemonic, so it imports anywhere.

**Why one key per name.** A name's owner is public, so an address that owns
several names links them, and that is as true inside one profile as across two.
A key per name leaves no such link. It also keeps an export narrow: a name's
secret is a leaf with no chain code, and its account index is hardened, so
handing it over hands over that name only, whatever else the holder has.

**Why hardened throughout.** BIP-85 uses hardened indexes only, so no extended
public key exists that could link two profiles, and a leaked profile mnemonic
derives no sibling. A name sits at the account level for the same reason, since
that is the level BIP-44 hardens: on a non-hardened index, an extended public
key above a name plus that name's private key yields the parent key and so every
other name in the profile, and a mnemonic exported to another wallet is exactly
how such an extended public key comes to exist. A profile is a compartment with
one way out, its own mnemonic, and a name inside it is another.

**Why 24-word children.** The same reason the master is 24: entropy cannot be
added to a seed afterwards, and what a seed is used for can grow. One length to
recognise rather than two. The word count is part of the path, so it is not a
setting and cannot change later.

**Why names start at 1.** The account level under a profile seed counts that
profile's names and nothing else, so account 0 is free, and it is kept for the
profile's own address at `m/44'/60'/0'/0/0`.

**What this does not buy.** A profile seed is derived from the master, so
whoever holds the master phrase derives every profile under it. This separates
what can be handed over, not what can be found.

## Profiles

A profile holds its seeds by index. Indexes come from a high-water mark and are
never reused: a profile the device no longer tracks still owns whatever its
names are registered to.

A profile holds no index, one, or several. Several is what a recovery leaves: a
device restored from the phrase alone has one profile and no idea which of the
recovered indexes belonged to which profile before, so they all land on the
first one and are moved out from there. At most one of a profile's indexes is
the one it buys new names under, recorded rather than inferred, so taking on a
recovered index never silently moves where the next name goes. A recovered index
arrives with no such mark, because a scan hands a profile many at once and only
one of them can carry it.

**A hidden profile cannot hold a name.** This is a check, not an omission. The
master derives every index under it, so unlocking any profile derives a hidden
profile's name keys as well, and the name is written into the profile's own row
and listed device-wide, while hidden profiles are a view filter rather than
encryption. Neither is closed by this layout, and closing them is work in the
profiles and in the name record rather than here. Until then a hidden profile is
given no index, by `bind` or by anything that follows it.

An incognito profile holds no seed and cannot hold a name.

## Commands

Internal API. The names commands will call these; users will not.

```
/_wallet                           whether the device has a seed, and the
                                   indexes the active profile holds
/_wallet create new                generate the master seed, 24 words
/_wallet create mnemonic=<phrase>  take the entropy from a mnemonic
/_wallet bind                      give the active profile the next free index to derive from master seed
/_wallet addresses profile=<p> from=<n> count=<c>
                                   name addresses n onwards under profile p
/_wallet export master             the master mnemonic
/_wallet export profile            the active profile's mnemonic, 24 words
/_wallet export name <n>           one name key's secret, 0x and 64 hex digits
/_wallet delete                    delete the master seed and its indexes
```

Creating and importing are one command, because they differ only in where the
entropy comes from, and either is refused if the device already has a master.
The source is always named, so no seed is generated by typing a prefix. A seed
is created only when asked for, never at startup and never as a side effect of
reading.

`bind` is how a profile gets the index it buys names under, and only a profile
that has none, so no profile is ever put on an index it did not ask for. A
profile can hold indexes and still have none to buy under, which is what a
recovery leaves, and `bind` serves it like any other. It is not done at startup
and not as a side effect of reading either.

`bind` allocates from the high-water mark, and after a phrase is imported that
mark is 0 again, so a restored device can bind an index that already owns names.
Nothing is spent, because buying is not here, but the binding is written, and
the scan that raises the mark is what reconciles it.

`/_wallet` answers what its name asks, whether the device has a seed and which
indexes the active profile holds, with the one it buys names under marked. It
derives no address, because an address is not what the bare command name
promises.

`addresses` is that derivation, and it is what a name scan is built on. It
answers with a path and an address per row, and `from=` with `count=` windows
the walk the way `count=` already windows a list of chat items. It does not need
the index to exist in the database: a scan after a lost database is looking for
indexes no row was ever written for, so the command derives from the seed and
the numbers alone.

There is one form and not a second one over profile indexes, because no single
address answers for a profile. Names are dense from 1, but a name can expire or
be transferred away, so the address at 1 can own nothing while 2 and 3 still do,
and a probe of 1 alone would skip a profile that holds names. The walk over
profile indexes is the walk over name indexes repeated, with the caller nesting
the same command.

`export` names what it exports, for the same reason `create` names its source:
the three subjects hand over very different amounts, and none of them should be
printed by typing a prefix. The master mnemonic is every profile, a profile
mnemonic is that profile's names and nothing else, a name secret is one name,
and the responses say which is which. `export` on its own is refused.

`export profile` and `export name` act on the index the active profile buys
under, and are refused where there is none, which is a profile that holds only
recovered indexes. Naming one of those is part of the command that moves a
recovered index to a profile, and lands with it.

The generated mnemonic is 24 words, 256 bits of entropy, rather than 12. That
buys nothing at the security level secp256k1 works at, and it is twice as much
to write down, so it is a deliberate choice rather than a default: entropy
cannot be added to a seed afterwards, and what a seed is used for can grow.

## Schema

```sql
CREATE TABLE wallet_seeds (
  wallet_seed_id INTEGER PRIMARY KEY AUTOINCREMENT,
  entropy BLOB NOT NULL,
  seed_kind TEXT NOT NULL,                       -- 'master' or 'profile'
  next_profile_index INTEGER NOT NULL DEFAULT 0  -- on the master row
);

CREATE TABLE wallet_profile_indexes (
  wallet_profile_index_id INTEGER PRIMARY KEY AUTOINCREMENT,
  wallet_seed_id INTEGER NOT NULL REFERENCES wallet_seeds ON DELETE CASCADE,
  profile_index INTEGER,                         -- null on a seed of the profile's own
  user_id INTEGER REFERENCES users ON DELETE SET NULL,
  buys_names INTEGER NOT NULL DEFAULT 0,
  next_name_index INTEGER NOT NULL DEFAULT 1
);

CREATE UNIQUE INDEX idx_wallet_seeds_master ON wallet_seeds(seed_kind) WHERE seed_kind = 'master';
CREATE UNIQUE INDEX idx_wallet_profile_indexes_index ON wallet_profile_indexes(wallet_seed_id, profile_index);
CREATE UNIQUE INDEX idx_wallet_profile_indexes_buys ON wallet_profile_indexes(user_id) WHERE buys_names = 1;
```

A seed is stored only when nothing derives it. The master is one such row, 16 to
32 bytes of BIP-39 entropy, and a profile seed derived from it is not stored at
all: it is computed when a key is needed, as the master seed itself already is.
`wallet_profile_indexes` holds what derivation cannot produce, which index a
profile was given, which seed it is under, which profile holds it, which of a
profile's indexes it buys names under, and how far each name counter has run.
The table is named for that, so that nothing in the schema reads as if a derived
seed were written down.

**A seed the master does not cover.** A profile can be put on entropy of its
own, imported rather than derived, and then the master phrase does not recover
that profile and must not appear to. `seed_kind` is what says so: a `master` row
is the device seed, and the only row `next_profile_index` means anything on; a
`profile` row is entropy that arrived on its own and serves exactly one profile.
The partial unique index keeps one master and leaves the number of profile rows
open. On a profile row `profile_index` is null, because there is no BIP-85 step
to record, the seed being the profile's root already. Importing one is not in
this change. The column is here so that adding it later reads rows written now
correctly, rather than having to guess what an unflagged seed was.

`users` is not touched. Which profile holds an index is on the index row,
because a recovery gives a profile several and a column on `users` could only
say one. `buys_names` picks the one new names go under, and the partial unique
index makes at most one per profile the database's rule rather than the
caller's. `bind` sets it, and nothing else does: a recovered index arrives
unmarked, so a profile that holds only those buys under none until it binds one
or is given one.

Deleting a profile leaves its indexes behind, unassigned and with the flag
meaningless until one of them is given to a profile again, because the names
under them are still registered to their addresses. Deleting a seed takes the
index rows under that seed with it, because an index without the entropy it
counts against derives nothing.

`next_name_index` counts what one seed holds, `next_profile_index` counts the
indexes handed out under the master, and both are high-water marks rather than
counts: an index is never reused.

Migrations are sorted by name, and the runner requires the ones a database has
applied to be a prefix of that list. A later wallet migration has to sort after
`20260908_wallet_seeds`, or every existing database fails to start.

## Recovery

Three cases, by how much of the database came back.

**The database is current.** It carries `wallet_profile_indexes`, so every
profile is back on its own index and nothing is asked of the user.

**The database is older than the seed.** It carries the profiles and the
bindings as of the backup, and nothing written after it: indexes bound since,
names bought since, and marks that are behind. What it is missing is the
phrase-only case over a smaller range, and the same scan answers it.

**Only the phrase is left.** Nothing in it says which indexes were used, and
nothing says which profile held which one. A scan of owned names answers the
first question, and it has two levels, a profile index and a name index under
it, each with a gap limit, so the number of probes is their product. That is a
real cost on a scan that already hands one registrar a view of the derivation
tree at the moment a user is restoring a lost device.

The scan is a nested walk over `addresses`, and the wallet supplies only the
addresses. For each profile index in turn the caller takes a window of that
profile's name addresses and asks the chain what they own, widening while names
keep appearing and moving on when a whole window owns nothing; the outer walk
ends after a gap of profile indexes that turned up nothing at all. Asking the
chain, counting the gap and raising the high-water marks are the names layer's,
which is why the wallet hands out addresses and counts nothing: a gap limit
needs the chain answers, and the wallet never sees them.

What the scan finds lands on the first profile, unmarked, which then holds
several indexes and buys under none of them until it binds one. The user moves
them out one at a time, or keeps one where it is and makes it the one that
profile buys under. That is a command, and it lands with the scan rather than
here, because a scan is what puts a profile in that state and nothing else can.
What it will do is move one index, with every name under it, to the profile that
should have it: nothing signed and nothing
moved on chain, because ownership does not change, only which profile the app
shows the names under. Pointing a name at the new profile's address stays a
separate signed record edit, the same edit that points a name anywhere else.

An index moves whole, because every name under it derives from one seed and an
index belongs to one profile at a time. Splitting one index's names between two
profiles would put a name under another profile's seed, which is an on-chain
transfer, and a name transfer is not a recovery step.

## What the phrase does not carry

A phrase carries entropy and nothing else.

**Which indexes are taken.** `next_profile_index` starts at 0 after an import
and `next_name_index` at 1, so until a scan raises them the next profile or the
next name would be taken at a path that already owns one. `bind` allocates from
the first of those and ships here, so a restored device can write a binding on
an index that already owns names. Nothing is spent, because buying is not here,
and the scan that raises the marks is what reconciles the binding; until it
lands the marks are a known stale value rather than a count of what the seed
holds.

**Which profile held which index.** Saying so by hand is the only answer, and
the command for it lands with the scan that finds the indexes in the first
place. Neither is here.

**A profile on a seed of its own.** Entropy that was imported rather than
derived is not in the phrase and cannot be, which is why `seed_kind` records it:
what the phrase recovers has to be answerable without guessing.

## Scope

Not here: buying a name, the names protocol, the registrar, signing, the
recovery scan and the command that moves a recovered index to a profile,
importing a seed for one profile and naming one in `addresses`, several master
seeds, the encrypted backup that would carry the index to profile map, a
profile's own address, stealth addresses.

`delete` takes the master and the indexes under it. A seed of a profile's own is
a row that nothing derives, so it is not under the master and is not taken with
it, which is one more thing importing has to settle when it lands.

Also not here: deriving at any layout but this one. A name may have been
registered from this phrase in another wallet, at the bare root or at a path
another tool picked, and finding those needs the caller to say which path rather
than which index. `addresses` takes indexes, so the candidate set of foreign
layouts arrives with the scan that needs it, as a widening of this command or
beside it.

## What is verified

Each of these is a test, not a claim.

- The BIP-85 vectors for application `39'` reproduce from the specification's
  master key, at 12, 18 and 24 words, entropy and mnemonic.
- A profile mnemonic, imported on its own as a master, reaches that profile's
  name addresses, so a profile can be handed to another wallet.
- Two profiles' addresses do not intersect, and neither mnemonic derives the
  other's.
- The BIP-44 addresses above reproduce, which is what an exported profile
  mnemonic gives in a wallet that enumerates Ledger Live accounts.
- A name path hardens its account component, so the path a name renders at ends
  in no index an extended public key could walk.
- `addresses` derives for an index the database has no row for, which is the
  case a scan exists for.
- `/_wallet` derives nothing.
- A secret whose first byte is zero keeps its 64 hex digits.
- Nothing is created until asked for, and a second create or import is refused.
- `bind` gives an index only to a profile with none to buy under, and no two
  profiles end up on one index.
- A profile buys names under at most one of its indexes, and the database
  refuses a second.
- `export profile` is refused for a profile that has no index to buy under.
- `export` on its own is refused, and each subject exports only itself.
- An index at or above 2^31 is refused rather than folded onto a low one.
- Binding a hidden profile is refused.
- The keys and the addresses survive a restart.
