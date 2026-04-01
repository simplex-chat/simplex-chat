# Plan: Agent API — getConnLinkPrivKey

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

3. Add implementation (near `deleteConnShortLink'`, ~line 1081):
   ```haskell
   getConnLinkPrivKey' :: AgentClient -> ConnId -> AM (Maybe C.PrivateKeyEd25519)
   getConnLinkPrivKey' c connId =
     withConnLock c connId "getConnLinkPrivKey" $ do
       SomeConn _ conn <- withStore c (`getConn` connId)
       pure $ case conn of
         ContactConnection _ rq -> linkPrivSigKey <$> shortLink rq
         RcvConnection _ rq -> linkPrivSigKey <$> shortLink rq
         _ -> Nothing
   ```

## Key points

- Local operation (no network IO) — synchronous, fast
- Uses `withConnLock` for consistency with other conn-level operations (e.g. `deleteConnShortLink'`)
- Returns `Maybe` — `Nothing` if connection has no short link credentials or is wrong type
- Handles both `ContactConnection` and `RcvConnection` (both have `RcvQueue` with `shortLink`)
- Chat layer signs: `C.sign' privKey challenge`
- Pattern follows `deleteConnShortLink'` (line 1081) — same conn lookup + pattern match

## Verification

```bash
cd simplexmq && cabal build --ghc-options=-O0
```
