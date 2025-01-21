# Group messages sending status

## Problem

Currently in UI chat item info:
- There's no differentiation between sent messages and pending messages.
- There's no differentiation between pending messages reasons (establishing connection or member inactivity).
  - Since the former is usually not a case due to group forwarding, this can be ignored.
- Messages to be forwarded by admin are not accounted.

## Solution

Differentiate new statuses for group sending in chat item info:
- forwarded
- inactive / pending

Option 1 is to add statuses to CIStatus / ACIStatus types.

Pros:
- simple.

Cons:
- further muddies type of statuses for chat item with impossible states / different dimension, as it's not applicable directly to chat item but a technicality of group sending process.

Option 2 is to create a new type, GroupSndStatus.

```haskell
data GroupSndStatus
  = GSSNew
  | GSSForwarded
  | GSSInactive
  | GSSSent
  | GSSRcvd {msgRcptStatus :: MsgReceiptStatus}
  | GSSError {agentError :: SndError}
  | GSSWarning {agentError :: SndError}
  | GSSInvalid {text :: Text}
```

Most statuses repeat CIStatus sending statuses, with addition of forwarded and inactive for group sending process.

Pros:
- separates concerns of chat item presentation from group sending process.
- allows future extension without further muddying CIStatus types.

Cons:
- more work.
- requires backwards compatible decoding with ACIStatus to read previous data from db.
