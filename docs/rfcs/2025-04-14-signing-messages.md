# Signing some member member messages and profiles

## Problems

1. Authenticity of profile addresses.

When somebody sends a profile, e.g. when connecting via one time link, it may include contact address. It may be incorrectly used to claim that the sending person is the same person as one who published the same address in some trustworthy channel, like a social media account or a website. This claim is unproven, and can potentially be misleading - the address can be simply copy-pasted, together with other attributes of the profile. The solution to that would be to sign the profile or a message including the profile with the key that has its public part included in immutable address data. If only profile is signed, it can also be copy-pasted, but if the message is signed and if it includes some connection attributes (e.g. security code), then it would be impossible to copy paste. So the latter approach is preferred.

Additional social media addresses can be included in the account, and they can also be validated via signature by the key from the short link published on social media account (requiring access to that account).

With Nostr that uses secp256k1 key as identity it's even better, as ownership of this identity can be validated without contacting nostr, simply by verifying signature with Nostr public key.

2. Authenticity of owners' and admins messages delivered via chat relays (aka superpeers).

For the next generation of groups we want to protect from attacks by chat relays who otherwise could deliver messages that they have no right to send. E.g., messages that can only be sent by owners and or by the admin.

Group identity is established by root Ed25519 key included into immutable part of group link data, and further ownership changes are recorded as an ownership chain, as described [here](https://github.com/simplex-chat/simplexmq/blob/master/rfcs/2025-04-04-short-links-for-groups.md#multiple-owners-managing-queue-data).

The same key should be used by owners to sign additions of admins and moderators, and these users would use their own keys communicated at the time they are added (and signed by owners or other admins). This chain of trust will be stored by admins, all members, and by chat relays, it should have strong consistency using some blockchain consensus algorithm, and should be re-sent on request, in case some messages were skipped or messages authenticated with missed key are delivered (in which case their processing would be postponed until the key is delivered).

This has been previously described, and is not the subject of this document. This document focus is how to sign messages in a verifiable way, which is problematic as signatures are applied to byte-streams, and we use JSON encoding with non-deterministic ordering of keys.

## Solution

### Option 1. Deterministic JSON encoding.

Include used key (or key reference), signature and some conversation binding (security code for direct chats, and, possibly, group root key for groups, as security code will not be shared between members) into the JSON being signed.

To sign:
- JSON will be deterministically encoded without signature
- signed,
- signature is added to nullable/Maybe field of the same object (or to wrapper object).

To verify:
- signature will be removed,
- remaining JSON re-encoded,
- signature verified.

Pros:
- backwards compatible (in case no wrapper is used),
- single-pass JSON decoding.

Cons:
- deterministic encodings are not widely used, and alternative implementations may fail to do it in a compatible way,
- requires re-encoding to verify signature, partially removing the advantages of the initial single-pass decoding.

### Option 2. Multi-stage decoding and encoding

As we need to sign the whole message anyway, and not just its part, it may be a new top-level encoding that sequentially includes:
- encoded JSON body (the order of keys is not important),
- conversation binding (security code or `group` and `member` identities defined as their keys),
- the array of tuples `(key reference, signature)`.

Key reference could point to:
- root key of the group,
- owner key in the chain of trust with root key,
- member key that is supposed to be previously known or included in profile in the message,
- nostr key included in the profile in the message.

To sign:
- encode JSON as usual, in non-deterministic way,
- append conversation binding,
- sign with all applicable keys,
- append `(key reference, signature)` tuples to the message,
- encode and send the message (could be JSON array or sequential SMP encoding),

To verify:
- decode message, obtaining JSON as string, conversation binding, and `(key reference, signature)` tuples,
- compare conversation binding, if it does not match - fail, notify the sender, show the message to the user (potentially, if sender sends the replacement, then warning could be replaced with the correct message).
- decode JSON string,
- resolve key references to keys (some can be in decoded message, some can be known),
- if any keys are unknown, request them from sender, show the warning to the user,
- verify signature, in case of failure notify sender (?) and show warning message to the user,
- if all ok, process the message.

Pros:
- more straightforward design,
- if binary encoding is used for message, then there will be no overhead of re-encoding JSON as a string (that would require escaping). The size increase can be offset by compression.

Cons:
- two-stage decoding may be seen as a downside, but it is offset by the fact that re-encodings are avoided, and under the hood JSON is decoded in stages anyway.

While deterministic JSON is [quite simple](https://github.com/simplex-chat/aeson/pull/4/files) for aeson implementation, the Option 2 seems more attractive overall, as it avoids questionable design of including signatures into JSON and the need to re-encode JSON to sign and to verify signatures.
