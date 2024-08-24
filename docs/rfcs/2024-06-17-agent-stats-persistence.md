# Agent stats persistence

## Problem

State/state tracked in agent are lost on app restart, which makes it difficult to debug user bugs.

## Solution

Persist stats between sessions.

App terminal signals may vary per platform / be absent (?) -> persist stats periodically.

Stats would have `<userId, server>` key, so we don't want to store them in a plaintext file to not leak used servers locally -> persist in encrypted db.

There's couple of orthogonal design decision to be made:
- persist in chat or in agent db
  - pros for chat:
    - possibly less contention for db than agent
  - pros for agent:
    - no unnecessary back and forth, especially if agent starts accumulating from past sessions and has to be parameterized with past stats (see below)
- agent to start accumulating from past sessions stats, or keep past separately and only accumulate for current session from zeros
  - pros for accumulating from past sessions:
    - easier to maintain stats - e.g. user deletion has to remove keys, which is more convoluted if past stats are not stored in memory
    - simpler UI - overall stats, no differentiation for past/current session (or less logic in backend preparing presentation data)
  - pros for accumulating from zeros:
    - simpler start logic - no need to restore stats from agent db / pass initial stats from chat db
    - can differentiate between past sessions and current session stats in UI

### Option 1 - Persist in chat db, agent to track only current session

- Chat stores stats in such table:

```sql
CREATE TABLE agent_stats(
  agent_stats_id INTEGER PRIMARY KEY, -- dummy id, there will only be one record
  past_stats TEXT, -- accumulated from previous sessions
  session_stats TEXT, -- current session
  past_started_at TEXT NOT NULL DEFAULT(datetime('now')), -- starting point of tracking stats, reset on stats reset
  session_started_at TEXT NOT NULL DEFAULT(datetime('now')), -- starting point of current session
  session_updated_at TEXT NOT NULL DEFAULT(datetime('now')) -- last update of current session stats (periodic, frequent updates)
);
```

- Chat periodically calls getAgentServersStats api and updates `session_stats`.
  - interval? should be short to not lose too much data, 5-30 seconds?
- On start `session_stats` are accumulated into `past_stats` and set to null.
- On user deletion, agent updates current session stats in memory (removes keys), chat has to do same for both stats fields in db.
  - other cases where stats have to be manipulated in similar way?

### Option 2 - Persist in chat db, agent to accumulate stats from past sessions

- Table is only used for persistence of overall stats:

```sql
CREATE TABLE agent_stats(
  agent_stats_id INTEGER PRIMARY KEY, -- dummy id, there will only be one record
  agent_stats TEXT, -- overall stats - past and session
  started_tracking_at TEXT NOT NULL DEFAULT(datetime('now')), -- starting point of tracking stats, reset on stats reset
  updated_at TEXT NOT NULL DEFAULT(datetime('now'))
);
```

- Chat to parameterize creation of agent client with initial stats.

### Option 3 - Persist in agent db, agent to differentiate past stats and session stats

- Table in agent db similar to option 1.
- Agent is responsible for periodic updates in session, as well as accumulating into "past" and resetting session stats on start.
- Agent only communicates stats to chat on request.
- On user deletion agent is fully responsible for maintaining both in-memory session stats, and updating db records.

### Option 4 - Persist in agent db, agent to accumulate stats from past sessions

- Table in agent db similar to option 2.
- On start agent restores initial stats into memory by itself.
- Since all stats are in memory, on user deletion it's enough to update in memory without updating db.
  - there is a race possible where agent crashes after updating stats (removing user keys) in memory before database stats have been overwritten by a periodic update, so it may be better to immediately overwrite and not wait for periodic update.
  - still at least there's at least no additional logic to update past stats.

### Other considerations

Why is it important to timely remove user keys from past stats?
- stats not being saved for past users:
  - important both privacy-wise and to not cause confusion when showing "All" stats (e.g. user summing up across users stats would have smaller total than total stats).
  - to avoid accidentally mixing up with newer users.
    - though we do have an AUTOINCREMENT user_id in agent so probably it wouldn't be a problem.
- on the other hand maybe we don't want to "forget" stats on user deletion so that stats would reflect networking more accurately?
