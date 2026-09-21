# Wallet keys

## Problem

The client needs blockchain keys of its own, separate from the messaging
protocol, at addresses it can derive again after a restart, after a database
restore, or on a new device. Otherwise whatever those addresses hold is lost
with the device.

The first use is names. A SimpleX name is a record on a public blockchain, the
record says which address owns the name, and anyone can read it. Every name gets
an account of its own, so the wallet has to hand out many accounts to one chat
profile. It is not a names wallet, though: an account is an ordinary Ethereum
account and can hold anything an address can hold, and later uses of this wallet
take accounts the same way.

Buying a name is not implemented yet. The keys land first, so the key material
can be reviewed on its own, before the names protocol, the registrar and
signing.

A device holds several chat profiles, and they are meant to stay apart. One
phrase that recovers the keys of all of them is one thing for the user to write
down, which is what a backup has to be. The layout below keeps the one phrase
and gives each profile a seed of its own to hand over instead, as a mnemonic.

Those seeds are not derived from anything about a profile. A profile is bound to
one of them by a row in the database, and no path mentions a profile, so the two
can be rearranged without moving a key.

## Words used here

**Entropy** is the random bytes a wallet is built from. A **mnemonic**, or
**phrase**, is those bytes written as BIP-39 English words, and the two convert
into each other exactly. A **seed** is what BIP-39 computes from a mnemonic, and
it is what keys are derived from. Entropy is stored; a seed is computed when a
key is needed.

A **path** such as `m/44'/60'/1'/0/0` walks down a BIP-32 tree. An apostrophe
marks a **hardened** step, which can only be taken with a private key, so no
public key above it can walk down to it.

A **chat profile** is one of the identities the app lets a user keep side by
side. A **hidden profile** is one the app shows only after a passphrase is
typed. An **incognito profile** is a throwaway identity for a single contact.
The **active profile** is the one the app is currently showing.

## Design

A device has one piece of BIP-39 entropy, the **master**, made as 24 words.
Nothing signs with it. Every key comes from it in two steps.

The first step is BIP-85, which turns one node of a BIP-32 tree into fresh
BIP-39 entropy. The entropy at **keyring index** `r` is a **keyring**: its own
24 words, and every key under them. A keyring is what a chat profile is given,
but it is not derived from the profile. Which profile has which keyring is a row
in the database and nothing more.

The second step is BIP-44 under a keyring. Each thing the keyring owns on chain
is given an **account index**, and the key at that index is the **account key**.
Names are the first use: one name, one account, and the account's address is
what the name record names as its owner.

```
master seed, 24 words                     the only thing to back up
└── keyring r  m/83696968'/39'/0'/24'/r'  BIP-85, its own 24-word mnemonic
    ├── m/44'/60'/n'/0/0                  accounts, n >= 0
    ├── m/5564'/60'/0'/0'/0               stealth spend               (later)
    └── m/5564'/60'/0'/1'/0               stealth view                (later)
```

The two path shapes in that tree both start at `m` because they are two trees,
not one. BIP-85 ends the first and starts the second.

BIP-85 derives entropy from a BIP-32 node by taking the bare 32-byte private key
of that node and computing `HMAC-SHA512("bip-entropy-from-k", k)`, where `k` is
that private key. The letter is the specification's and appears in the string it
fixes; it is not the keyring index. For 24 words the entropy is the leading 32
bytes of the result. The rest of the path says which mnemonic is wanted: `39'`
is BIP-85's BIP-39 application, `0'` is English, `24'` is the word count, and
`r'` is the keyring index. Every step of it is hardened.

Those 32 bytes are BIP-39 entropy, not a seed. They become 24 words, the words
become a seed the BIP-39 way, and that seed is the root of the profile's BIP-32
tree. Feeding the 32 bytes straight into BIP-32 would work inside the app and
would silently break every claim below about another wallet.

Under a keyring this is plain BIP-44, and an account index is BIP-44's own
account level, the level Ledger Live varies and also calls an account. So a
keyring phrase given to another wallet reaches the same addresses, and the two
tools use the word for the same thing, although Ledger Live numbers them from
one, where its Account 1 is index 0.

That wallet will find account 0 by itself and stop there. BIP-44 says to stop
looking at the first account with no transaction history, and an account that
only owns a name never transacts, so discovery goes no further. Account 0 is
shown anyway, because a wallet always offers the first one. For any account
above it the user has to enter the path.

The tests pin BIP-85 against the vectors in its own specification, and BIP-44
against the standard `abandon ... about` test mnemonic, which is 12 words, with
an empty BIP-39 passphrase. Addresses are in EIP-55 mixed case.

```
account 0   m/44'/60'/0'/0/0   0x9858EfFD232B4033E47d90003D41EC34EcaEda94
account 1   m/44'/60'/1'/0/0   0x78839F6054d7ed13918bAe0473BA31b1Ca9D7265
```

**Why a seed for each profile.** Without one, what a user can hand over is all
or nothing. The master is every profile. A single account key is one account.
Neither is "this profile's accounts", and a keyring is, in a form another
wallet can take.

**Why an account for each name.** A name's owner address is public, so an
address that owns two names links them. That is as true inside one profile as
across two, and an account for each name leaves no such link. The same holds for
anything else an account comes to own.

**Why the account level is hardened.** BIP-32 has a trap. For a step that is
not hardened, the extended public key above a key, together with that one key's
private half, yields the parent private key and from it every sibling. Giving a
keyring to another wallet is how such an extended public key comes to
exist, and giving away one account key is the other half. The two steps below an
account are not hardened, so those two halves do reach that account's own key,
and nothing else is derived under an account today. The account level itself is
hardened, so they stop there and reach no other account. BIP-85 is hardened at
every step, so they reach no other profile either.

**Why accounts start at 0.** Nothing is reserved. Account 0 is an account like
any other, and it is the one address every wallet shows for a phrase without
being asked to look, so the first account a profile takes is the first account
another wallet offers.

**Why 24 words.** Entropy cannot be added to a seed afterwards, and what a seed
is used for can grow. 256 bits buys nothing against secp256k1 and is twice as
much to write down, so it is a deliberate choice rather than a default. For a
keyring the word count is part of the BIP-85 path, so it is fixed for good.

**What this does not buy.** A keyring is derived from the master, so
whoever holds the master phrase derives every profile. The design changes what a
user can hand over. It does not hide one profile from someone who already holds
the phrase.

## Profiles and keyrings

A chat profile is bound to one keyring at a time, and a keyring to one profile.
The binding is a row, so it can be changed without any key moving, and nothing
about the profile reaches the derivation.

Most profiles have no keyring. One is bound on first use, when a profile buys a
name, rather than when the profile is made, so a user who never buys anything
has an empty wallet and a device that has never derived a key.

Keyring indexes are handed out in order and never reused, because a keyring the
device no longer tracks still owns whatever its accounts hold. A keyring can
sit unbound, holding accounts that no profile currently reaches.

**A hidden profile binds no keyring, so it cannot own a name.** Two things would
leak. First, the master derives every keyring under it, so unlocking any profile
on the device also derives a hidden profile's account keys. Second, a name is
written into the profile's own database row and listed across the device, while
a hidden profile is a filter on what is shown, not encryption. Closing either is
work in the profiles and in the name record, not in the key layout.

An incognito profile is meant to leave nothing behind, and a binding is
something left behind, so an incognito profile binds none either.

## Commands

An internal API, called by the names commands and by whatever else takes
accounts later, rather than typed by users. Nothing here runs at startup or as a
side effect of reading, so no key material is made or handed out unless it was
asked for.

```
/_wallet                           whether the device has a master, and which
                                   keyring the active profile is bound to, and
                                   whether the master derives that keyring
/_wallet create new                generate the master, 24 words
/_wallet create mnemonic=<phrase>  take the master entropy from a phrase
/_wallet bind keyring=<r>          bind the active profile to a keyring; without
                                   keyring=, the lowest unbound one, or a new
                                   one when there is none
/_wallet address keyring=<r> account=<n>
                                   one address, with the keyring and account
                                   index it came from; without account=, the
                                   next free account; without keyring=, the one
                                   the active profile is bound to
/_wallet export master             the master phrase
/_wallet export keyring            the bound keyring's phrase, 24 words
/_wallet export account <n>        one account key's secret, 0x and 64 hex
/_wallet delete                    delete the master entropy and its keyrings
```

**create.** Generating and importing are one command, because they differ only
in where the entropy comes from. The source is always named, so no key material
is generated by typing a prefix. A second one is refused while the device has a
master. An imported phrase must be 12, 15, 18, 21 or 24 BIP-39 English words
with a valid checksum; case and spacing are normalised and anything else is
refused. There is no BIP-39 passphrase, because it would be a second secret to
back up and losing it would look exactly like losing the phrase. A phrase that
was used with one elsewhere therefore lands on a different tree here and finds
nothing, with nothing to say why.

**bind.** The active profile is bound to a keyring. Named with `keyring=<r>` it
takes that one, which is how a profile that was bound to the wrong one is put
right. Left to itself it takes the lowest keyring that exists and is unbound,
and only makes a new one, from a counter on the master, when there is none. That
order is what makes a recovery work without anyone naming anything.

The counter is a high-water mark and not a count of what is held. A profile that
already has a keyring is refused, since a profile has one at a time; rebinding
it means naming the new one, which says plainly that the old one is being let
go.

BIP-32 hardens an index by adding 2^31 to it, so an index at or above 2^31 wraps
into one that is not hardened. That is a silent loss of hardening rather than a
collision, and it would put a whole profile back in reach of the attack the
account level stops. Every index this API takes is refused at or above 2^31,
both the keyring index and the account index.

After an import the counter is unknown rather than zero, because the phrase does
not say how many keyrings it has already been used for. Making a new keyring is
refused while it is unknown, so an imported phrase cannot make one that already
has accounts in use. Binding an existing keyring is still allowed, which is the
whole of what a recovered device does. Only a search of the chain can set the
counter, and it is not in this change.

**address.** This is where derivation is asked for. It answers with one path and
one address, and with the keyring index and account index they came from, so a
caller that left either out knows what it got. `keyring=` left out means the
keyring the active profile is bound to, and is refused when it is bound to none.
`account=` left out means the next free account, which is what
the counter says and is a reading rather than a claim: the command takes nothing
and writes nothing, and asking twice gives the same answer. It is refused where
there is no counter to read, which is a keyring index with no row and an index
whose counter is still unknown.

With `keyring=` given, the keyring index does not have to exist in the database.
A device that lost its database is looking for indexes no row was ever written
for, so the command works from the master entropy and the two numbers alone.

One address at a time is all this needs to be. A caller sweeping the tree loops
over both numbers itself, and there is deliberately no form that sweeps keyring
indexes for it, because no single address answers for a keyring: what an account
holds can go away, so account 0 can hold nothing while 1 and 2 still hold
names.

**export.** Each form names what it exports, for the same reason `create` names
its source. The three hand over very different amounts. The master phrase is
every keyring. A keyring phrase is every account under one keyring, and grows
as branches are added to the layout. An account key's secret is one account. All
three are deterministic, so each also hands over what has not been made yet:
accounts taken later, and keyrings made later. `export` with no argument is
refused. `export keyring` and `export account` act on the keyring the active
profile is bound to, and are refused for a profile bound to none.

An export is a copy and not a handover. The device still derives what it
exported and can still sign with it, so giving an account key away leaves two
parties able to act as its owner until whatever it holds is transferred on
chain, which is not in this change. Signing is not in this change either, and
when it lands it is a command here that signs and returns a signature, not
`export account` followed by signing elsewhere. A key that left for routine work
would make the narrow export the ordinary path and waste the point of having
one.

**delete.** The master entropy and its index rows go. Names already registered
stay registered to their addresses, and only the phrase reaches them again, so a
user who has not written the phrase down loses them. Nothing else in the
database is touched.

Every refusal says which it is: no master, a master already exists, a bad
phrase, a hidden profile, a profile that already has an index, a profile with no
index, an unknown counter, or an index out of range.

## Recovery

Three cases, by how much of the database came back.

**Only the phrase is left.** A phrase carries entropy and nothing else. Two
things are not in it and cannot be. The first is which indexes were used: the
master's counter starts out unknown, as does the account counter of any index
found later, and only a search of the chain sets them. The second is which
profile was bound to which keyring: nothing recovers that, and the order the
user makes profiles in is what stands in for it.

That search is a nested walk over `address`, one call for each candidate. For
each keyring index in turn, the caller asks for account 0, then 1, and so on,
and asks what each address owns. It keeps going while accounts turn out to hold
something and moves on after a run of accounts that hold nothing. The walk over
keyring indexes ends after a run of indexes that hold nothing at all. Asking
what an address owns, deciding how long a run has to be, and recording the
answers belong to the caller, because the wallet never sees an answer.

Two things about that question are unsettled. Going from an address to the names
it owns is not something a registry answers on chain, so it takes the registrar
or an indexer, and which one decides who sees the search. And the answer covers
what is owned now, while the search is asked which indexes were ever used, so an
index whose names have all expired or moved on looks unused. The search can
therefore under-report, and a user who knows better has to be able to send it
further.

Nothing outside this API writes these tables, so the search does not set the
counters itself. It reports what it found, and a command of this API that lands
with it writes a row for each keyring found, unbound, and moves the master's
counter past them.

What the search finds keeps its own keyring index. Nothing is attached to a
profile, and the user is asked nothing. As the user makes chat profiles again
and each one buys its first name, `bind` takes the lowest unbound keyring, so
profiles made in the same order as before end up on the keyrings they had. That
is the whole of the normal case: the user does not have to remember which
keyring was which, only to make the profiles in the order they made them
before.

A different order puts a profile on a keyring that was another one's, which
shows up as the wrong names. `bind keyring=<r>` fixes it, and fixing it moves no
key and signs nothing, because ownership does not change, only which profile the
app shows the accounts under. Pointing a name at the new profile's address is a
separate signed edit of the name's record.

A keyring binds whole. Every account under it comes from one seed, so splitting
its accounts between two profiles would mean putting an account under another
keyring. That is an on-chain transfer of whatever the account holds, not a
recovery step.

**The database is older than the master.** It has the profiles and the indexes
as of the backup, and nothing written after it: indexes bound since, names
taken since, and counters that are behind. A counter that is behind is worse
than one that is unknown, because it looks usable, so such a database is treated
as having unknown counters until the same search fills the gap, over a smaller
range.

**The database is current.** It records which profile is bound to which keyring,
so every profile is back on its own and nothing is asked of the user.

## Schema

```sql
CREATE TABLE wallet_seeds (
  wallet_seed_id INTEGER PRIMARY KEY AUTOINCREMENT,
  entropy BLOB NOT NULL CHECK (length(entropy) IN (16, 20, 24, 28, 32)),
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

Only entropy that nothing can derive is stored. The master is such a row, 16 to
32 bytes, which is 12 to 24 words: generation always makes 24, and a shorter
import is still accepted. A keyring's own entropy is never stored, because the
master entropy and a keyring index derive it whenever a key is needed.

So `wallet_keyrings` holds what derivation cannot produce: which keyring index
exists, which stored entropy it belongs to, which profile is bound to it, and
how far its account counter has run. A row is a keyring the device knows about,
and a row with no `user_id` is one no profile currently reaches.

`users` is not touched. The binding lives on the keyring row, and the unique
index on `user_id` is what makes one keyring for each profile a rule of the
database rather than of the caller. Null user ids are distinct, so any number of
keyrings can sit unbound.

Deleting a chat profile leaves its keyring row, unbound, because the accounts
under it still hold whatever they held. Deleting stored entropy takes the
keyring rows that belong to it, because a keyring index with no entropy behind
it derives nothing.

The migration has no reverse step. Dropping `wallet_seeds` would destroy the
only copy of the master entropy, and with it everything the device owns.

**Entropy the master does not cover.** A keyring can be put on entropy of its
own, imported rather than derived. The master phrase then does not recover it,
and the schema must not suggest otherwise. `seed_kind` says which is which: a
`master` row is the device's own, and the only row `next_keyring_index` applies
to; a `keyring` row is entropy that arrived on its own, and its single keyring
row records a null `keyring_index`, there being no BIP-85 step to write down.
Importing is not implemented here. The column exists so that a row written now
can be read correctly later, rather than leaving an unmarked row to be guessed
at.

A null `keyring_index` means the row's entropy is the keyring's root, with no
BIP-85 step. A keyring row must therefore never combine a null `keyring_index`
with the master row, or `export keyring` would print the master phrase. No
constraint across two tables can say so in SQLite, so the rule is enforced where
rows are written and there is a test for it.

## Known limits

These are accepted for this change, not oversights.

1. **The phrase alone does not restore a device.** It recovers keys, not which
   keyrings were used or which profile had which. Until the search lands, an
   imported phrase gives a device its keys and no way to find what they own.
2. **The search tells one party a great deal.** It asks about every address the
   phrase could hold a name on, in one burst, so it hands whoever answers a view
   of the derivation tree and undoes much of what a key for each name buys. This
   needs an answer before the search ships.
3. **The same phrase on two devices collides.** The counter lives in one
   database, so two devices holding one phrase both make the same keyring and
   each believes it is free. Sharing the counter needs a backup both can read.
4. **A profile hidden after it was bound keeps its keyring.** The check happens
   at bind time, so it does not catch a profile that is bound first and hidden
   afterwards.
5. **A deleted chat profile leaves its keyring unbound, and the next profile to
   bind takes it.** So deleting a profile and making another hands the new one
   the old one's accounts and names. That is what makes a recovery work without
   anyone naming a keyring, and it is the wrong answer for a user who deleted a
   profile to be rid of it.
6. **Nothing records which layout a seed was used under.** A phrase used in
   another wallet may hold accounts at paths this document does not describe.
7. **An export answers over the link that asked.** These commands are not
   blocked from a paired device, so `export master` can put the phrase on a
   second device. Only the command text is kept out of logs; the answer is not.
8. **The database file is the phrase.** The stored entropy converts back into
   the words, so anything that copies the database copies every key on the
   device, including an archive exported to move devices. No export granularity
   helps against someone who copies one file, and nothing here asks for a
   passphrase or a confirmation before an export or a delete. `delete` does not
   overwrite either, so a deleted row survives in free pages and in the journal,
   and `delete` followed by `create mnemonic=` swaps the wallet for another one
   with nothing recording that it happened.
9. **A wallet that publishes extended public keys links a profile's names.**
   Giving a keyring phrase to a wallet that sends account keys to a vendor hands
   that vendor every name in the profile at once, which is what a key for each
   name otherwise prevents.
10. **`address` ignores profiles.** It derives for any index straight from
    the master, so any caller can enumerate every profile's addresses, hidden
    ones included. The search needs that, and nothing here narrows it.
11. **Keyring indexes are not dense.** A keyring can be made and never used,
    and a run of empty keyrings is how the search decides to stop, so a keyring
    far above a gap can be missed.
12. **A short import gives strong looking children.** A keyring phrase is always
    24 words, but it carries only what the master carries, so a 12-word master
    yields 24-word keyring phrases with 128 bits behind them.
13. **Gas and discovery pull against each other.** If a name address ever pays
    for anything, whatever funds it links every name in the profile on chain.
    If it never pays, it never transacts, which is why no wallet finds it.
14. **Purpose `5564'` is ours.** It is reserved here for stealth keys, taken
    from an ERC number rather than registered as a BIP-43 purpose, and nothing
    in this change derives at it.

## Scope

Not here: buying a name, the names protocol, the registrar, signing, the search
of the chain and the command that records what it found, importing
entropy for one profile, more than one master, a shared backup of the counters,
and an address of the profile's own.

Not here either: deriving at any layout but this one. A name may have been
registered from this phrase in another wallet, at the bare master key or at a
path some other tool picked. Finding those means the caller says which path
rather than which index, and `address` takes indexes, so the set of layouts to
try arrives with the search that needs it.

BIP-85 is implemented in this project's crypto library, on top of the BIP-32 and
BIP-39 code already there, and adds no dependency, because the HMAC-SHA512 it
needs is the one BIP-32 already uses.

The master entropy is a row in the chat database and is protected exactly as
that database is. A phrase reaches the API twice, in the text of
`create mnemonic=` and in the answer to `export`, and the command text is kept
out of logs.

## What is verified

Each of these is a test, not a claim.

- The BIP-85 vectors for application `39'` reproduce from the master key in its
  specification, at 12, 18 and 24 words, entropy and mnemonic.
- The two addresses above reproduce from the `abandon ... about` mnemonic.
- A master phrase reaches an account address end to end, through BIP-85, through
  a
  real BIP-39 import of the words it produced, and on to `m/44'/60'/1'/0/0`, so
  the BIP-39 step between the two trees cannot be skipped unnoticed.
- A keyring phrase, imported as an ordinary BIP-39 mnemonic, reaches that
  profile's account addresses.
- Two profiles' first ten account addresses do not intersect, and neither
  profile's phrase derives the other's.
- An account path hardens its account component.
- `address` derives for a keyring index the database has no row for, and with
  no `account=` returns the counter's value rather than moving it, twice
  running.
- `address` with no `keyring=` uses the active profile, and is refused when the
  counter is unknown and no `account=` was given.
- `/_wallet` returns no address.
- An account key's secret whose first byte is zero keeps its 64 hex digits.
- A second generate or import is refused, and an imported phrase of a wrong
  length, a wrong word or a bad checksum is refused.
- `bind` with no argument takes the lowest unbound keyring, and only makes a new
  one when there is none.
- `bind` refuses to make a new keyring on an imported master, and still binds an
  existing one.
- `bind` is refused on a hidden profile and on a profile already bound, and
  `bind keyring=<r>` rebinds it.
- Profiles made in the same order after a recovery end up on the keyrings they
  had.
- Every index the API takes is refused at or above 2^31, both the keyring index
  and the account index.
- No two profiles are bound to one keyring, and no profile to two.
- `export` with no argument is refused, and `export keyring` is refused for a
  profile bound to none.
- `export keyring` never returns the master phrase, whatever a keyring row
  says.
- Keys and addresses survive a restart.
