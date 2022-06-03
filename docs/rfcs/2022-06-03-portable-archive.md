# Portable archive file format

## Problems

- database migration for notifications support
- export and import of the database

The first problem could have been solved in an ad hoc way, but it may cause data loss, so the proposal is to have migration performed via export/import steps.

Out of scope of this doc - what will be the UX for database migration. It may be fully automatic, via code, with zero user interactions, or it could be via step by step wizard - irrespective of this choice it would include export and import steps.

# Proposal

Implement creating archive file and restoring from the archive in Haskell, application would only provide a source and target folders, respectively

Archive files structure:

- simplex_v1_chat.db
- simplex_v1_agent.db
- simplex_v1_files
  - ...

Archive file name (includes UTC time):

simplex-chat.YYYY-MM-DDTHH:MM:SSZ.zip
