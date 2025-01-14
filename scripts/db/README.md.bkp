# Transfer data from SQLite to Postgres database

1. Prepare postgres database. \*

   Build `simplex-chat` executable with `client_postgres` flag and run it for the first time.

   This should create `simplex_v1` database with `agent_schema` and `chat_schema` schemas, and `migrations` tables populated. Some tables would have initialization data.

2. Delete data from all tables other than service and `migrations` tables. \*

   For example, run via DBeaver:

   ```sql
   DO $$
   DECLARE
      table_name text;
   BEGIN
      EXECUTE 'SET SEARCH_PATH TO agent_schema';

      FOR table_name IN
         SELECT tablename
         FROM pg_catalog.pg_tables
         WHERE schemaname = 'agent_schema'
            AND tablename NOT LIKE 'pg_*'
            AND tablename != 'migrations'
      LOOP
         EXECUTE format('DELETE FROM %I;', table_name);
      END LOOP;
   END $$;
   ```

   Repeat for `chat_schema`.

   Now you should have `simplex_v1` database with `agent_schema` and `chat_schema` schemas, and `migrations` tables populated, without any data in other tables.

   To check number of rows for all tables in schema run:

   ```sql
   WITH    tbl AS (
   SELECT Table_Schema, Table_Name
   FROM   information_schema.Tables
   WHERE  Table_Name NOT LIKE 'pg_%'
         AND Table_Schema IN ('agent_schema')
   )
   SELECT  Table_Schema AS Schema_Name
   ,       Table_Name
   ,       (xpath('/row/c/text()', query_to_xml(format(
            'SELECT count(*) AS c FROM %I.%I', Table_Schema, Table_Name
         ), FALSE, TRUE, '')))[1]::text::int AS Records_Count
   FROM    tbl
   ORDER   BY Records_Count DESC;
   ```

   Repeat for `chat_schema`.

   All tables except `migrations` tables should have 0 rows.

3. Export data from SQLite via `export_sqlite.sh` script.

   ```sh
   ./scripts/db/export_sqlite.sh <sqlite_db_prefix> <output_folder>
   ```

   This should export data into csv files with table names inside `agent_db` and `chat_db` subfolders of the output folder.

4. Import data to Postgres database.

   ```sh
   ./scripts/db/import_postgres.sh <exported_tables_folder> <postgres_user> <postgres_database>
   # For example
   ./scripts/db/import_postgres.sh <exported_tables_folder> postgres simplex_v1
   ```

   Note: user should have privilege to copy from files, for example it can be admin `postgres` user. Exported tables folder is output folder from previous step.

\* Alternatively, it's possible to create schemas manually, and insert first migration rows also manually.
