# Advanced SimpleX Chat bot example

In most cases [a simple REPL bot](../simplex-bot/) is sufficient, but in cases you want to program more advanced communication scenarios you may take a more complex event-based approach, as in this example.

Event-based approach allows you:

- decide whether to connect to a user or not depending on any factors, e.g. user display name.
- disconnect from users who send too many messages or send messages that bot finds inappropriate.
- react to message deletions and editing.
- process reply messages differently, taking the original message into account.
- process and send images and voice messages.
- create groups of users, e.g. to connect 2 users.
- etc.
