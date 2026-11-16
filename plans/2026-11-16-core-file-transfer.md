# Files: received and stored files that never complete

One pull request for three core fixes where a file transfer never completes. Each fix has its own plan and was reviewed in its own pull request; this plan states the problems, the approach of each fix and the decisions open for review.

## Problems

| # | Problem as users meet it | Fix branch | Plan |
|---|---|---|---|
| P1 | A received XFTP file stays at "receiving" with 100% progress, and the saved file is empty, when the temporary folder and the files folder are on different file systems (CLI and bots with a tmpfs `/tmp`, desktop with a symlinked files folder) | nd/xftp-rcv-cross-device-rename (#8018) | 2026-10-18-rcv-file-cross-device-rename.md |
| P2 | In a group, a received file can wait forever on "Waiting for file" while the sender shows it as uploaded | nd/forwarded-file-no-descr (#8019) | 2026-11-03-group-file-description-late-members.md |
| P3 | A desktop or CLI controller with host file encryption fails to send a file to a remote host when its temporary folder and its remote hosts folder are on different file systems | nd/remote-file-cross-device-rename | 2026-10-28-remote-store-file-cross-device.md |

## Approach

- P1: when the rename of the downloaded file to its target fails, the file is copied and the source removed.
- P2: the sender sends a file's description to every member that received the file, directly or once connected, instead of only to members connected when it sent the file.
- P3: when the rename of the encrypted file into the remote hosts folder fails, the file is copied and the source removed.

## Decisions for review

1. Order of merge: one pull request (recommended), or one per problem.
2. P1 and P3 handle the same failure, a rename across file systems, with the same fallback written twice: P1 ignores a failed removal of the source, P3 does not. One function that moves a file, renaming or else copying and removing, used at both sites, is recommended before the final review.

## Tests

Each fix adds a test that fails on master: tests/ChatTests/Files.hs, tests/ChatTests/Groups.hs and tests/RemoteTests.hs. CI: the reduced build with the file, group and remote tests.
