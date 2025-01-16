# Transfer data from SQLite to Postgres database

1. \* Decrypt SQLite database if it is encrypted.

   ```sh
   sqlcipher encrypted_simplex_v1_agent.db
   ```

   ```sql
   PRAGMA key = 'password';
   ATTACH DATABASE 'simplex_v1_agent.db' AS plaintext KEY '';
   SELECT sqlcipher_export('plaintext');
   DETACH DATABASE plaintext;
   ```

   Repeat for `simplex_v1_chat.db`.

2. Prepare Postgres database.

   Build `simplex-chat` executable with `client_postgres` flag and run it to initialize new Postgres chat database.

   This should create `simplex_v1` database with `agent_schema` and `chat_schema` schemas, and `migrations` tables populated. Some tables would have initialization data - it will be truncated via pgloader command in next step.

3. Load data from decrypted SQLite databases to Postgres database via pgloader.

   Install pgloader and add it to PATH.

   ```sh
   SQLITE_DBPATH='simplex_v1_agent.db' POSTGRES_CONN='postgres://simplex@/simplex_v1' POSTGRES_SCHEMA='agent_schema' pgloader --on-error-stop sqlite.load

   SQLITE_DBPATH='simplex_v1_chat.db' POSTGRES_CONN='postgres://simplex@/simplex_v1' POSTGRES_SCHEMA='chat_schema' pgloader --on-error-stop sqlite.load
   ```

4. Update sequences for Postgres tables.

   ```sql
   DO $$
   DECLARE
      rec RECORD;
   BEGIN
      EXECUTE 'SET SEARCH_PATH TO agent_schema';

      FOR rec IN
         SELECT
            table_name,
            column_name,
            pg_get_serial_sequence(table_name, column_name) AS seq_name
         FROM
            information_schema.columns
         WHERE
            table_schema = 'agent_schema'
            AND identity_generation = 'ALWAYS'
      LOOP
         EXECUTE format(
            'SELECT setval(%L, (SELECT MAX(%I) FROM %I))',
            rec.seq_name, rec.column_name, rec.table_name
         );
      END LOOP;
   END $$;
   ```

5. Compare number of rows between Postgres and SQLite tables.

   To check number of rows for all tables in Postgres database schema run:

   ```sql
   WITH tbl AS (
      SELECT table_schema, table_name
      FROM information_schema.Tables
      WHERE table_name NOT LIKE 'pg_%'
        AND table_schema IN ('agent_schema')
   )
   SELECT
      table_schema AS schema_name,
      table_name,
      (xpath('/row/c/text()', query_to_xml(
         format('SELECT count(*) AS c FROM %I.%I', table_schema, table_name), false, true, ''
      )))[1]::text::int AS records_count
   FROM tbl
   ORDER BY records_count DESC;
   ```

   Repeat for `chat_schema`.
