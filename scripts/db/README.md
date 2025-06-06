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

   - Create Postgres database. In shell:

      ```sh
      createdb -O simplex simplex_v1
      ```

      Or via query.

   - Build `simplex-chat` executable with `client_postgres` flag and run it to initialize new chat database.

      This should create `simplex_v1_agent_schema` and `simplex_v1_chat_schema` schemas in `simplex_v1` database, with `migrations` tables populated. Some tables would have initialization data - it will be truncated via pgloader command in next step.

3. Load data from decrypted SQLite databases to Postgres database via pgloader.

   Install pgloader and add it to PATH. Run in shell (substitute paths):

   ```sh
   SQLITE_DBPATH='simplex_v1_agent.db' POSTGRES_CONN='postgres://simplex@/simplex_v1' POSTGRES_SCHEMA='simplex_v1_agent_schema' pgloader --on-error-stop sqlite.load

   SQLITE_DBPATH='simplex_v1_chat.db' POSTGRES_CONN='postgres://simplex@/simplex_v1' POSTGRES_SCHEMA='simplex_v1_chat_schema' pgloader --on-error-stop sqlite.load
   ```

4. Update sequences for Postgres tables.

   ```sql
   DO $$
   DECLARE
      rec RECORD;
   BEGIN
      EXECUTE 'SET SEARCH_PATH TO simplex_v1_agent_schema';

      FOR rec IN
         SELECT
            table_name,
            column_name,
            pg_get_serial_sequence(table_name, column_name) AS seq_name
         FROM
            information_schema.columns
         WHERE
            table_schema = 'simplex_v1_agent_schema'
            AND identity_generation = 'ALWAYS'
      LOOP
         EXECUTE format(
            'SELECT setval(%L, (SELECT MAX(%I) FROM %I))',
            rec.seq_name, rec.column_name, rec.table_name
         );
      END LOOP;
   END $$;
   ```

   Repeat for `simplex_v1_chat_schema`.

5. \* Compare number of rows between Postgres and SQLite tables.

   To check number of rows for all tables in Postgres database schema run:

   ```sql
   WITH tbl AS (
      SELECT table_schema, table_name
      FROM information_schema.Tables
      WHERE table_name NOT LIKE 'pg_%'
        AND table_schema IN ('simplex_v1_agent_schema')
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

   Repeat for `simplex_v1_chat_schema`.

6. Build and run desktop app with Postgres backend.

   Run in shell (paths are from project root):

   ```sh
   ./scripts/desktop/build-lib-mac.sh arm64 postgres

   ./gradlew runDistributable -Pdatabase.backend=postgres
   # or
   ./gradlew packageDmg -Pdatabase.backend=postgres
   ```
