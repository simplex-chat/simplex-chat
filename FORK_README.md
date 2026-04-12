# SimpleX Chat Fork: Hard Expiry

This fork adds **hard expiry** (wall-clock message deletion) to SimpleX Chat groups.

## What it does

Every group message gets an absolute UTC deletion timestamp stamped at send time. When the clock reaches that timestamp, the message is deleted on all clients — whether or not it was ever read.

This solves the problem of messages persisting indefinitely on devices that are lost, abandoned, or belong to deceased members. The existing disappearing messages feature only triggers after a message is read; hard expiry provides an unconditional backstop.

## How it differs from upstream

One new feature, minimal changes:

- **New DB column**: `hard_expiry_at` on `chat_items` (indexed)
- **New group preference field**: `hardExpiryDuration` on `TimedMessagesGroupPreference`
- **New wire protocol field**: `hardExpiryAt` on `ExtMsgContent` (optional, backward compatible)
- **Sweep**: integrated into the existing `cleanupManager` periodic loop
- **CLI commands**: `/set expiry #group <duration|off>`, `/show expiry #group`

No existing behavior was changed. The disappearing messages system is untouched.

## Building

Requires GHC 9.6.3 and cabal. On macOS with Homebrew:

```bash
# Install GHC
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
ghcup install ghc 9.6.3
ghcup set ghc 9.6.3

# Install OpenSSL
brew install openssl@3

# Build
cd simplex-chat
cabal update
DYLD_FALLBACK_LIBRARY_PATH=/opt/homebrew/opt/openssl@3/lib \
  cabal build exe:simplex-chat -j1 \
  --extra-lib-dirs=/opt/homebrew/opt/openssl@3/lib \
  --extra-include-dirs=/opt/homebrew/opt/openssl@3/include

# Run
DYLD_FALLBACK_LIBRARY_PATH=/opt/homebrew/opt/openssl@3/lib \
  cabal exec simplex-chat
```

## Design

See [docs/PR-PROPOSAL.md](docs/PR-PROPOSAL.md) for the full design, backward compatibility analysis, and file change list.
