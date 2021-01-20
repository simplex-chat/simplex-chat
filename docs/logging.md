# SMP agent logging

## Problem and proposed solution.

SMP agent performs multiple actions in response to the client commands and to the messages received from other SMP agents (wrapped in SMP protocol messages).

Customary approach for the network protocol clients is to have a debug/verbose mode that enables logging of all sent and received messages and any other actions that the client performs.

This document proposes a logging format for SMP agent that would be enabled if the agent is run with `--verbose` / `-v` command line option.

We can also consider logging the database operations that change the data.

## Types of actions and the associated log line format.

### Client connected / disconnected

```
client n connected to Agent
client n disconnected from Agent
```

where `n` is a sequential number of a connected agent client, starting from 1 (over the agent run-time).

### Server connected / disconnected

```
Agent connected to host:port
Agent disconnected from host:port
```

### Received command from the client

```
n --> A : corrId connAlias parsed_command // raw_command
```

`raw_command` is added only in case of parsing failure.

### Sent command to SMP server

```
A --> host:port : corrId queueId parsed_command
```

### Received response / message from the SMP server

```
A <-- host:port : corrId queueId parsed_command // raw_command
```

In case the response is a message or notification, corrId should be replaced with `_`

### Interpreted ("unwrapped") SMP message as agent message

```
Agent msg : connAlias parsed_message // raw_message 
```

### Sent response / message to the client

```
n <-- A : corrId connAlias parsed_command // raw_command
```

### Database changes

```
DB : insert/delete/update table key
DB : insert/delete/update table key
```