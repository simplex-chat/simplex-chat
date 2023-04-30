# Group connections recovery after asynchronous commands

## Problem

Similar to direct chat connections, group connections can fail to be established on failing IO, for example on bad network. When this happens agent throws error. For direct connections it is viable to propagate the error to UI to indicate failure, so the user can retry. For group connections this becomes unviable as multiple connections are being established automatically on each new joining member (2 for each existing member except host), and the user can't retry.

## Proposal 2

- Have separate command processing queue in agent

- Chat creates correlation id for command

- Chat has data structure to represent possible continuations

- Chat saves correlation id and continuation to database

- Agent asynchronously responds after command completion with correlation id

- Chat restores continuation by correlation id and processes it

## Proposal

- Add special versions of commands `createConnection`, `joinConnection` (`allowConnection` as well?), to which agent responds synchronously with connection id and status;

- Add new connection type `NewConnection` / `NoQueueConnection`;

- Add agent responses signalling these commands' asynchronous completion, e.g. `CREATE_SUCCESS`, `JOIN_SUCCESS` (?);

- Return connection status on subscriptions;

In chat:

- Event-driven processing - for group connections use commands with synchronous responses, save intermediate connection state, process success responses;

- On subscription check whether connection's status has changed, if yes, run respective continuation - same as on success event.

### Commands use

`joinConnection` in group connections is used:

- #779 : when joining group - `APIJoinGroup`;
- #2147 : on receiving `XGrpMemFwd` message for both group and direct connections; -- from inviting member to existing members

`allowConnection`:

- #1419 : on receiving `XGrpMemInfo` in `CONF` inside direct connection; -- from existing member to invitee after XGrpMemFwd
- #1486 : on receiving `XGrpAcpt` in `CONF` inside group connection; -- from invitee to host
- #1495 : on receiving `XGrpMemInfo` in `CONF` inside group connection; -- from existing member to invitee after XGrpMemFwd

`createConnection`:

- #756 : when inviting new member - `APIAddMember`, probably doesn't have to be recovered - error can be signalled;
- #2112 : on receiving `XGrpMemIntro` message for both group and direct connections for each introduced member; -- from host to invitee

## Misc

Chat.hs #1419 - why is XOk sent instead of XGrpMemInfo?
