# Test Chat Relay Implementation Notes

## CRITICAL INSTRUCTIONS (reload after compaction)
- **NO DELEGATION**: Do not use Task tool. Context is critical.
- **ADVERSARIAL SELF-REVIEW**: 2 consecutive clean passes required before reporting done.
- **BUILD**: `cabal build --ghc-options=-O0`
- **TEST**: `cabal test simplex-chat-test --test-options='-m "chat relays"'` and `cabal test simplex-chat-test --test-options='-m "channels"'`
- **MINIMIZE USER INTERACTION**: Work autonomously, report divergences at end.
- **Plan file**: `plans/2026-04-01-test-chat-relay-plan.md`
- **iOS Swift changes ARE in scope** — implement for end-to-end testing
- **Kotlin is NOT in scope** — deferred until Swift passes review

## Phase Status
- [x] Phase 1: Protocol types (RelayProfile, RelayAddressLinkData, XGrpRelayTest)
- [x] Phase 2: UserChatRelay type change (name → relayProfile)
- [x] Phase 3: Controller types (RelayTest, RelayTestFailure, commands, response)
- [x] Phase 5: Commands.hs handler (APITestChatRelay)
- [x] Phase 6: Subscriber.hs handlers (owner CONF, relay REQ)
- [x] Phase 7: Store (createRelayTestConnection)
- [x] Phase 8: APICreateMyAddress relay link data
- [x] Phase 9: Cleanup stale test connections
- [x] Phase 10: iOS Swift views
- [x] Phase 11: View.hs output
- [x] Phase 12: CLI parsing
- [x] Phase 13: Tests
- [x] Adversarial self-review pass 1
- [x] Adversarial self-review pass 2

## Divergences from Plan
1. **CRChatRelayTestResult field name**: Plan uses `testFailure :: Maybe RelayTestFailure`, but GHC rejects duplicate field names with different types in same ADT (`CRServerTestResult` already has `testFailure :: Maybe ProtocolTestFailure`). Changed to `relayTestFailure :: Maybe RelayTestFailure`.
2. **TH splice ordering**: Plan places `deriveJSON` for `RelayTestStep`/`RelayTestFailure` inline with type definitions. This splits the TH declaration group, making `ChatError`, `ChatDeleteMode` etc. invisible to earlier declarations. Moved deriveJSON calls to the deriveJSON area (after `ChatError` derivation, before `ChatResponse` derivation).
3. **NumericUnderscores**: Plan uses `40_000_000` but GHC extension not enabled. Changed to `40000000`.
4. **PatternSynonyms in Direct.hs**: Added `{-# LANGUAGE PatternSynonyms #-}` pragma to import `PQSupportOff`.
5. **OperatorTests.hs**: Test file used old `UserChatRelay {name}` pattern — updated to `UserChatRelay {relayProfile = RelayProfile {name}}` and added `RelayProfile` import.
6. **Plan `decodeSig` approach**: Plan uses `<$?>` operator for `JQ.Parser`. Actual implementation uses `Either String` monad from `p`/`opt` context in Protocol.hs JSON parsing. Both achieve same result.
7. **drgRandomBytes helper**: Plan uses manual `gVar <- asks random; challenge <- liftIO $ atomically $ C.randomBytes 32 gVar`. Used existing `drgRandomBytes 32` helper from Internal.hs instead.
8. **Missing API-level parser**: Plan only specified CLI parser `/relay test`. Added `"/_relay test "` API parser for iOS Swift integration (needed for `apiTestChatRelay` command).
9. **Test fix for `/da` output**: `viewUserContactLinkDeleted` outputs TWO lines. Added missing `bob <## "To create a new chat address use /ad"` assertion after the first `/da` output line.
10. **Swift backward compatibility**: Plan suggested changing `name` to `relayProfile` in Swift `UserChatRelay`. Instead added a computed `name` property that delegates to `relayProfile.name`, preserving all existing callsites.

## Key Patterns Discovered
- TH splices create declaration group boundaries — types must be defined before splices that reference them
- `DuplicateRecordFields` forbids same-name fields with different types in one ADT
- `PatternSynonyms` extension needed to import pattern synonyms
- `<##.` operator in test utils does prefix matching (replaces plan's `startsWith`)
- `peerConnChatVersion` takes two `VersionRangeChat` args, defined in Types.hs

## Build/Test Log
- Phase 2 complete: build clean
- Phase 3 first attempt: TH ordering error, fixed by moving deriveJSON calls
- Phase 3 second attempt: field name conflict, fixed by renaming to `relayTestFailure`
- Phase 3 third attempt: build clean
- Phase 5+7: NumericUnderscores error, fixed. PatternSynonyms error, fixed.
- All Haskell phases: library builds clean
- OperatorTests.hs: old field name error, fixed
- Awaiting full test suite build and test run
- Phase 10 (iOS Swift): All types and API function added, builds not verified (no Xcode in env)
- Full test run: 7 chat relay tests PASS, 9 operator tests PASS, 35 channel tests PASS
- Adversarial self-review pass 1: CLEAN (JSON encoding/decoding, type safety, race conditions, bindings)
- Adversarial self-review pass 2: CLEAN (all checks repeated, no issues found)
- **STATUS: COMPLETE** — all phases implemented, all tests pass
