# Transfer data from SQLite to Postgres database

1. Prepare postgres database.

   Build `simplex-chat` executable with `client_postgres` flag and run it for the first time, without creating user (terminate at user creation).

   This should create `simplex_v1` database with `agent_schema` and `chat_schema` schemas created, and `migrations` tables populated, without any data in other tables.

2. Export data from SQLite via `export_sqlite.sh` script.

   ```sh
   ./scripts/db/export_sqlite.sh <sqlite_db_prefix> <output_folder>
   ```

   This should export data into csv files with table names inside `agent_db` and `chat_db` subfolders of the output folder.

3. Import data to Postgres database.

   ```sh
   ./scripts/db/import_postgres.sh <exported_tables_folder> <postgres_user> <postgres_database>
   # For example
   ./scripts/db/import_postgres.sh <exported_tables_folder> postgres simplex_v1
   ```

   Note: user should have privilege to copy from files, for example it can be admin `postgres` user. Exported tables folder is output folder from previous step.
