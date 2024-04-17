# IP address protection, support for SMP sending proxies

## Problem

IP addresses of senders being visible to recipients' chosen servers, which in case of self-hosting makes it visible to recipients themselves. In case of XFTP files the issue is reversed, with IP addresses of recipients being visible to senders' chosen servers.

## Solution

### SMP

- Agent to support sending proxies
- New network settings configuration "Use SMP proxies"
  - Can be set to "always", "never", "for unknown servers"
  - Currently configured servers to be considered as "known", including those disabled for new connections
  - Initial default is "never" to allow opt-in for testing, to be changed to "for unknown servers" later
  - Support in UI
- No alerts about unknown servers are required in UI
- Tor setting to not affect agent decision to use SMP proxy for each given server

``` haskell
data UseSMPProxies
  = SMPPAlways
  | SMPPNever
  | SMPPUnknown
  deriving (Eq, Show)

data NetworkConfig = NetworkConfig
  { ...
    useSMPProxies :: UseSMPProxies,
    ...
  }
```

### XFTP

Some considerations:

- XFTP proxying is not planned to be implemented initially
  - In future it could be done via open socket and with no persistance, and would be required only for FGET command
- Currently XFTP files are automatically set for reception in some cases:
  - Images and voice messages
  - In background (via "set to receive")
- Agent resumes file reception (download) after app re-start, in case they weren't fully downloaded
- User may change tor setting between and during app sessions, so a file can be "accepted" when app is connected via tor, but resumed when it's no longer

Solution:

- Client would make a decision whether to automatically accept file, or alert user about unknown servers
- Add only_via_tor flag to agent rcv_files + pass through APIs:
  - add Bool to `ReceiveFile` (with False meaning user hasn't indicated any intent regarding unknown servers)
    - can either be automatically accepted, or require alerting user and asking for confirmation (see below)
  - add `onlyViaTor` to `xftpReceiveFile`
- Possible scenarios:
  1. XFTP file chunks are fully available on known servers
    - Do not show alert, accept automatically (images, voice messages) or without approval (other files)
    - `onlyViaTor` is False
  2. Some file chunks are on unknown servers, tor is not enabled
    - User accepts manually, show alert to user
    - User confirms, indicating it's Ok to proceed even though IP would be visible to unknown servers
    - `onlyViaTor` is False
    - \* Additional user preference to never ask? -> behavior same as now
  3. Some file chunks are on unknown servers, tor is enabled
    - Do not show alert, accept automatically (images, voice messages) or without approval (other files)
    - `onlyViaTor` is True
- On file download:
  - `onlyViaTor` equal False is ignored
  - If `onlyViaTor` is True, and tor is not enabled, throw new "permanent" error
    - Permanent for simplicity as this is an edge case
    - File record to be removed from agent
    - Could analyze whether it's remaining (not yet downloaded) chunks are on unknown servers, and only abort in this case (and instead proceed if remaining chunks are on known servers)
      - This requires changing download to load data for all chunks instead of loading only current chunk
      - This is another edge case of edge case, as it seems more likely that sender either used only self-hosted servers, or only preset servers
      - So it seems as unnecessary complication, and it should be Ok to abort simply based on only_via_tor flag + tor setting
    - Error is RFERR XFTP UNKNOWN_NO_PROXY (new constructor)
- Chat to differentiate RFERR, and make file available for re-download on UNKNOWN_NO_PROXY error, similar to when file is cancelled
  - New CIFileStatus - CIFSRcvCancelledNoProxy, to differentiate in UI
  - Different icon for retry, alert explaining why file was aborted

### Trusted servers

We also considered an idea of trusted servers, but it proved to have unnecessarily complex UX, and in case of XFTP had issues on file download continuation after restart (e.g. server "trusted" flag changing, automatically accepting with tor enabled). It requires additional consideration and may be better suited to concept of "server providers".
