# Transfer data from SQLite to Postgres database

1. Decrypt SQLite database if it is encrypted.

   - Agent:

      Open sqlite db:

      ```sh
      sqlcipher simplex_v1_agent.db
      ```

      Set your db password:

      ```sql
      PRAGMA key = '<your_password>';
      ```

      Check if db was successfully decrypted:

      ```sh
      SELECT count(*) FROM sqlite_master;
      ```

      Attach new empty db:

      ```sh
      ATTACH DATABASE 'simplex_v1_agent_plaintext.db' AS plaintext KEY '';
      ```

      Export opened db to attached db as plaintext:

      ```sh
      SELECT sqlcipher_export('plaintext');
      ```

      Deattach the plaintext db:

      ```sh
      DETACH DATABASE plaintext;
      ```

   - Chat:

      Open sqlite db:

      ```sh
      sqlcipher simplex_v1_chat.db
      ```

      Set your db password:

      ```sql
      PRAGMA key = '<your_password>';
      ```

      Check if db was successfully decrypted:

      ```sh
      SELECT count(*) FROM sqlite_master;
      ```

      Attach new empty db:

      ```sh
      ATTACH DATABASE 'simplex_v1_chat_plaintext.db' AS plaintext KEY '';
      ```

      Export opened db to attached db as plaintext:

      ```sh
      SELECT sqlcipher_export('plaintext');
      ```

      Deattach the plaintext db:

      ```sh
      DETACH DATABASE plaintext;
      ```

2. Prepare Postgres database.

   1. Connect to PostgreSQL databse:

      ```sh
      psql -U postgres -h localhost
      ```

   2. Create user with password:

      ```sh
      CREATE USER simplex WITH ENCRYPTED PASSWORD '123123';
      ```

   3. Create database:

      ```sh
      CREATE DATABASE simplex_v1;
      ```

   4. Assign permissions:

      ```sh
      GRANT ALL PRIVILEGES ON DATABASE simplex_v1 TO simplex;
      ```

3. Prepare database:

   Build CLI with PostgreSQL support:

   ```sh
   cabal build -fclient_postgres exe:simplex-chat
   ```

   Execute CLI:

   ```sh
   ./simplex-chat -d "postgresql://simplex:123123@localhost:5432/simplex_v1" --create-schema
   ```

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
