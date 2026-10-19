# Supporter Badges v1 - Verification

Badge verification in stable so that v6.5 users can see and verify badges from v7 users. Badge purchase and issuance is v2.

## Why BBS+

BBS+ signatures (IETF draft-irtf-cfrg-bbs-signatures) allow a holder of a signed credential to generate zero-knowledge proofs that selectively disclose some signed attributes while hiding others. Each proof uses a random nonce, making different proofs from the same credential computationally unlinkable - a verifier seeing two proofs cannot determine they came from the same credential. This means a supporter badge shown to different contacts cannot be correlated, preserving SimpleX's unlinkable identity model.

The server that signs the credential sees the master secret during signing but cannot link any received proof back to any signing session - this is the core zero-knowledge property.

## References

- IETF draft: https://datatracker.ietf.org/doc/draft-irtf-cfrg-bbs-signatures/
- libbbs: https://github.com/Fraunhofer-AISEC/libbbs (Apache-2.0, Fraunhofer-AISEC)
- blst: https://github.com/supranational/blst (Apache-2.0, audited by NCC Group) - internal dependency of libbbs for BLS12-381 curve operations

Both are vendored verbatim into simplexmq so that users and maintainers can verify the source matches upstream. Only libbbs API is called directly.

## Crypto

3 signed messages: `[ms, expiry, level]`. `ms` undisclosed (index 0), `expiry` and `level` disclosed (indexes 1, 2). Proof size: 304 bytes (272 base + 32 per undisclosed).

Server public key (`srvPK`, 96 bytes) hardcoded in app.

## libbbs integration

Vendor libbbs + blst C sources into simplexmq. Haskell FFI bindings following the SNTRUP761 pattern (`Simplex.Messaging.Crypto.BBS.Bindings`).

Full FFI surface for testing the complete flow:

- `bbs_keygen_full` - generate keypair
- `bbs_sign` - sign messages
- `bbs_proof_gen` - generate ZK proof with selective disclosure
- `bbs_proof_verify` - verify proof
- `bbs_sha256_ciphersuite` - ciphersuite constant

Unit tests: keygen, sign, proof gen, proof verify roundtrip. Verify proof size. Verify rejection of tampered proofs. Verify two proofs from same credential don't correlate (different presentation headers produce different proofs that both verify).

Use blst portable C fallback for now (avoids per-arch assembly).

## Profile type

Add optional `badge` field to `Profile`. The `SupporterBadge` type uses base64-encoded newtypes for binary fields, following the `KEMPublicKey`/`KEMCiphertext` pattern from SNTRUP761 bindings:

```haskell
data SupporterBadge = SupporterBadge
  { proof :: BBSProof
  , proofNonce :: ByteString
  , badgeExpiry :: UTCTime
  , badgeType :: Text
  }
```

`badgeType` is a string: `"supporter"`, `"business"`, `"legend"`, `"cf_investor"`. Displayed in UI as Supporter, Business, Legend, Crowdfunding Investor. `BBSProof` is a newtype over `ByteString` with `StrEncoding` instances for base64url JSON encoding.

Backward compatible: `omitNothingFields` means older clients ignore it, newer clients without badge send `Nothing`.

## DB

- `badge` fields on `contact_profiles` and `group_member_profiles` to store received badge data
- `badge_status` column on `contacts` and `group_members` to store verification result
- `badge` fields on user profile (`users` or `contact_profiles` for own profile) for when badge issuance is added in v2

## Verification

On receiving profile with `badge` (in Subscriber.hs, `XInfo`/`XGrpMemInfo`/`XContact` handlers):

1. `bbs_proof_verify(srvPK, proof, "", proofNonce, disclosed=[1,2], [expiry, level])`
2. Check `expiry >= now`
3. Store badge + verification status on contact/member

## UI

Badge icon next to display name for verified contacts/members. Different icons per level string. Expired badges shown differently or hidden.

## Not in v1

- Badge purchase, issuance, credential storage, proof generation - v2
- Service framework - v2
- Payment platform integration - v2
