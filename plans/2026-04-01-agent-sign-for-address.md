# Plan: Agent API — getConnLinkPrivKey

**Date: 2026-04-01**

## Context

The chat relay test (`APITestChatRelay`) requires the relay to sign a challenge with its address private key (`ShortLinkCreds.linkPrivSigKey`). This key is stored in the agent's database on `RcvQueue` and is not accessible from the chat layer. A new agent API function is needed to retrieve it.

The chat layer performs the signing itself with `C.sign'`.

## API

```haskell
getConnLinkPrivKey :: AgentClient -> ConnId -> AE (Maybe C.PrivateKeyEd25519)
```

- `ConnId` — the agent connection ID
- Returns — `Just linkPrivSigKey` if the connection has short link credentials, `Nothing` otherwise

## Implementation

**File: `simplexmq/src/Simplex/Messaging/Agent.hs`**

1. Add to module exports:
   ```haskell
   getConnLinkPrivKey,
   ```

2. Add public function (near `getConnShortLink`, ~line 427):
   ```haskell
   getConnLinkPrivKey :: AgentClient -> ConnId -> AE (Maybe C.PrivateKeyEd25519)
   getConnLinkPrivKey c = withAgentEnv c . getConnLinkPrivKey' c
   {-# INLINE getConnLinkPrivKey #-}
   ```

3. Add implementation (near `deleteConnShortLink'`, ~line 1089):
   ```haskell
   getConnLinkPrivKey' :: AgentClient -> ConnId -> AM (Maybe C.PrivateKeyEd25519)
   getConnLinkPrivKey' c connId = do
     SomeConn _ conn <- withStore c (`getConn` connId)
     pure $ case conn of
       ContactConnection _ rq -> linkPrivSigKey <$> shortLink rq
       RcvConnection _ rq -> linkPrivSigKey <$> shortLink rq
       _ -> Nothing
   ```

## Design notes

- Local operation (no network IO) — synchronous, fast
- No `withConnLock` — this is a pure read with no mutations; the lock would add latency for no benefit. Read-only agent operations like `getConn` don't require the conn lock.
- Returns `Maybe` — `Nothing` if connection has no short link credentials or is wrong type
- Handles both `ContactConnection` and `RcvConnection` (both have `RcvQueue` with `shortLink` field, Store.hs:159)
- Chat layer signs: `C.sign' privKey challenge`
- `linkPrivSigKey :: C.PrivateKeyEd25519` on `ShortLinkCreds` (Protocol.hs:1456)
- `shortLink :: Maybe ShortLinkCreds` on `StoredRcvQueue` (Store.hs:159)

## Verification

```bash
cd simplexmq && cabal build --ghc-options=-O0
```
