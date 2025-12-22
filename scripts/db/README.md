## Transfer data from SQLite to Postgres database

1. Decrypt SQLite database if it is encrypted.

   1. Agent:

      1. Open sqlite db:

      ```sh
      sqlcipher simplex_v1_agent.db
      ```

      2. Run in sqlcipher:

      ```sql
      PRAGMA key = '<your_password>'; -- Set your db password
      SELECT count(*) FROM sqlite_master; -- Check if db was successfully decrypted
      ATTACH DATABASE 'simplex_v1_agent_plaintext.db' AS plaintext KEY ''; -- Attach new empty db
      SELECT sqlcipher_export('plaintext'); -- Export opened db to attached db as plaintext
      DETACH DATABASE plaintext;
      ```

   2. Chat:

      1. Open sqlite db:

      ```sh
      sqlcipher simplex_v1_chat.db
      ```

      2. Run in sqlcipher:

      ```sql
      PRAGMA key = '<your_password>';
      SELECT count(*) FROM sqlite_master;
      ATTACH DATABASE 'simplex_v1_chat_plaintext.db' AS plaintext KEY '';
      SELECT sqlcipher_export('plaintext');
      DETACH DATABASE plaintext;
      ```

2. Prepare Postgres database.

   1. Connect to PostgreSQL databse:

      ```sh
      psql -U postgres -h localhost
      ```

   2. Run in psql:

      ```sql
      CREATE USER simplex WITH ENCRYPTED PASSWORD '123123'; -- Create user with password
      -- or
      -- CREATE USER simplex;
      CREATE DATABASE simplex_v1; -- Create database
      GRANT ALL PRIVILEGES ON DATABASE simplex_v1 TO simplex; -- Assign permissions
      ```

3. Prepare database:

   You should build the CLI binary from the same `TAG` as the desktop.

   1. Build CLI with PostgreSQL support:

      ```sh
      cabal build -fclient_postgres exe:simplex-chat
      ```

      And rename it to:

      ```sh
      mv simplex-chat simplex-chat-pg
      ```

   2. Execute CLI:

      ```sh
      ./simplex-chat-pg -d "postgresql://simplex:123123@localhost:5432/simplex_v1" --create-schema
      ```

      Press `Ctrl+C` when CLI ask for a display name.

   This should create `simplex_v1_agent_schema` and `simplex_v1_chat_schema` schemas in `simplex_v1` database, with `migrations` tables populated. Some tables would have initialization data - it will be truncated via pgloader command in next step.

3. Load data from decrypted SQLite databases to Postgres database via pgloader.

   Install pgloader and add it to PATH. Run in shell (substitute paths):

   ```sh
   export POSTGRES_CONN='postgresql://simplex:123123@localhost:5432/simplex_v1'
   ```

   And then:

   ```sh
   SQLITE_DBPATH='simplex_v1_agent_plaintext.db' \
   POSTGRES_SCHEMA='simplex_v1_agent_schema' \
   CPU_CORES=$(nproc) WORKERS=$((CPU_CORES - 1)) pgloader --dynamic-space-size 262144 --on-error-stop sqlite.load

   SQLITE_DBPATH='simplex_v1_chat_plaintext.db' \
   POSTGRES_SCHEMA='simplex_v1_chat_schema' \
   CPU_CORES=$(nproc) WORKERS=$((CPU_CORES - 1)) pgloader --dynamic-space-size 262144 --on-error-stop sqlite.load
   ```

4. Update sequences for Postgres tables.

   Connect to db:

   ```sh
   PGPASSWORD=123123 psql -h localhost -U simplex -d simplex_v1
   ```

   Execute the following:

   1. For `agent`:

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

   2. For `chat`:

      ```sql
      DO $$
      DECLARE
         rec RECORD;
      BEGIN
         EXECUTE 'SET SEARCH_PATH TO simplex_v1_chat_schema';

         FOR rec IN
            SELECT
               table_name,
               column_name,
               pg_get_serial_sequence(table_name, column_name) AS seq_name
            FROM
               information_schema.columns
            WHERE
               table_schema = 'simplex_v1_chat_schema'
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

   **PostgreSQL**:

   1. For `agent`:

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

   2. For `chat`:

      ```sql
      WITH tbl AS (
         SELECT table_schema, table_name
         FROM information_schema.Tables
         WHERE table_name NOT LIKE 'pg_%'
           AND table_schema IN ('simplex_v1_chat_schema')
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

   **SQLite**:

   1. For `agent`:

      ```sh
      db="simplex_v1_agent_plaintext.db"
      sqlite3 "$db" "SELECT name FROM sqlite_master WHERE type='table';" |
      while read table; do
         count=$(sqlite3 "$db" "SELECT COUNT(*) FROM \"$table\";")
         echo "$table: $count"
      done | sort -k2 -nr | less
      ```

   2. For `chat`:

      ```sh
      db="simplex_v1_chat_plaintext.db"
      sqlite3 "$db" "SELECT name FROM sqlite_master WHERE type='table';" |
      while read table; do
         count=$(sqlite3 "$db" "SELECT COUNT(*) FROM \"$table\";")
         echo "$table: $count"
      done | sort -k2 -nr | less
      ```

6. Build and run desktop app with Postgres backend.

   Run in shell (paths are from project root):

   ```sh
   ./scripts/desktop/build-lib-mac.sh arm64 postgres

   ./gradlew runDistributable -Pdatabase.backend=postgres
   # or
   ./gradlew packageDmg -Pdatabase.backend=postgres
   ```

## Transfer data from Postgres to SQLite database

1. Prepare sqlite db:

   1. Download simplex-chat CLI:

      You should download the CLI binary from the same `TAG` as the desktop.

      ```sh
      export TAG='v6.4.3.1'
      curl -L "https://github.com/simplex-chat/simplex-chat/releases/download/${TAG}/simplex-chat-ubuntu-22_04-x86_64" -o 'simplex-chat'
      ```

   2. Run the CLI:

      ```sh
      ./simplex-chat
      ```

      Press `Ctrl+C` when CLI ask for a display name.

   3. Move database:

      ```sh
      mv ~/.simplex/simplex_v1_* ~/.local/share/simplex/
      ```

2. Transfer data:

   ```sh
   ./pg2sqlite.py --verbose 'postgresql://simplex:123123@localhost:5432/simplex_v1' ~/.local/share/simplex/
   ```

4. Update BLOBs:

   ```sh
   sqlite3 simplex_v1_chat.db
   ```

   ```sh
   UPDATE group_members SET member_role = CAST(member_role as BLOB);
   UPDATE user_contact_links SET group_link_member_role = CAST(group_link_member_role AS BLOB) WHERE group_link_member_role is not null;
   ```
