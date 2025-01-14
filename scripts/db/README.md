# Transfer data from SQLite to Postgres database

1. Prepare postgres database.

   - Build `simplex-chat` executable with `client_postgres` flag and run it for the first time.

      This should create `simplex_v1` database with `agent_schema` and `chat_schema` schemas, and `migrations` tables populated. Some tables would have initialization data.

   - Delete data from all tables other than service and `migrations` tables.

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

   \* Alternatively, it's possible to create schemas manually, and insert first migration rows also manually.

2. Create SQLite dumps.

   ```sh
   sqlite3 simplex_v1_agent.db ".dump" | grep "^INSERT INTO" | grep -v "^INSERT INTO migrations" | grep -v "^INSERT INTO sqlite_sequence" > sqlite_agent_dump.sql
   sqlite3 simplex_v1_chat.db ".dump" | grep "^INSERT INTO" | grep -v "^INSERT INTO migrations" | grep -v "^INSERT INTO sqlite_sequence" > sqlite_chat_dump.sql
   ```

3. Transform for Postgres.

   ```sh
   sed -E "s/X'([0-9A-Fa-f]*)'/DECODE('\1','hex')/g; s/(INSERT INTO \"?[a-zA-Z0-9_]+\"?) VALUES/\\1 OVERRIDING SYSTEM VALUE VALUES/g" sqlite_agent_dump.sql > postgres_agent_inserts.sql
   sed -E "s/X'([0-9A-Fa-f]*)'/DECODE('\1','hex')/g; s/(INSERT INTO \"?[a-zA-Z0-9_]+\"?) VALUES/\\1 OVERRIDING SYSTEM VALUE VALUES/g" sqlite_chat_dump.sql > postgres_chat_inserts.sql
   ```

4. Disable constraints on all tables.

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
         EXECUTE format('ALTER TABLE %I DISABLE TRIGGER ALL;', table_name);
      END LOOP;
   END $$;
   ```

   Repeat for `chat_schema`.

5. Insert data into Postgres.

   ```sh
   psql "user=postgres dbname=simplex_v1 options=--search_path=agent_schema" --set ON_ERROR_STOP=on -q -f postgres_agent_inserts.sql
   psql "user=postgres dbname=simplex_v1 options=--search_path=chat_schema" --set ON_ERROR_STOP=on -q -f postgres_chat_inserts.sql
   ```

6. Enable constraints on all tables.

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
         EXECUTE format('ALTER TABLE %I ENABLE TRIGGER ALL;', table_name);
      END LOOP;
   END $$;
   ```

   Repeat for `chat_schema`.

7. Update sequences for Postgres tables.

8. Compare number of rows between Postgres and SQLite tables.
