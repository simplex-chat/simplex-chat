# SimpleX Services

## Background and motivation

SimpleX clients communicating with SimpleX services require message format separate from SimpleX Chat `x` namespace.

## Proposal

Use `s` (for "service") namespace with REST like messages for requests and responses, e.g.

```
s.post resource data
s.get resource
...
```

Requests only require resource locations at the service, location of the service itself is determined by the SMP connection.

Responses can either have separate messages:

```
s.ok
s.not_found
...
```

Or be united under a single "response" envelope:

```
s.resp.ok
s.resp.not_found
...
```

## Schema migration

## Implementation plan
