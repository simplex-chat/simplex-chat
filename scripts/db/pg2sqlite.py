#!/usr/bin/env python3
"""
PostgreSQL -> per-schema SQLite migration with colored, clean logging.

Usage example:
  python db_migrate.py 'postgresql://user:pass@host:5432/db' /path/to/sqlite/dir --dry-run

Note: color output will be disabled automatically if stdout is not a TTY or if --no-color is passed.
"""

from __future__ import annotations
import argparse
import logging
import os
import sqlite3
import sys
import re
import datetime
from pathlib import Path
from typing import Set, List, Tuple, Dict, Optional, NamedTuple

import psycopg
from psycopg import sql

# ----------------------
# Small utilities
# ----------------------

try:
    sqlite3.register_adapter(
        datetime.datetime,
        lambda v: v.isoformat(sep=" ", timespec="microseconds"),
    )
    sqlite3.register_adapter(datetime.date, lambda v: v.isoformat())
    sqlite3.register_adapter(
        datetime.time, lambda v: v.isoformat(timespec="microseconds")
    )
except ValueError:
    pass  # already registered

ANSI = {
    "reset": "\x1b[0m",
    "bold": "\x1b[1m",
    "dim": "\x1b[2m",
    "red": "\x1b[31m",
    "green": "\x1b[32m",
    "yellow": "\x1b[33m",
    "blue": "\x1b[34m",
    "magenta": "\x1b[35m",
    "cyan": "\x1b[36m",
    "gray": "\x1b[90m",
}

DEFAULT_BATCH_SIZE = 10000

TYPE_COMPATIBILITY = {
    "bytea": ["BLOB", "CHAR", "CLOB", "TEXT", "JSON"],
    "int": ["INT", "NUMERIC"],
    "serial": ["INT", "NUMERIC"],
    "numeric": ["NUMERIC", "DECIMAL", "REAL", "FLOAT", "DOUBLE"],
    "decimal": ["NUMERIC", "DECIMAL", "REAL", "FLOAT", "DOUBLE"],
    "real": ["NUMERIC", "DECIMAL", "REAL", "FLOAT", "DOUBLE"],
    "double": ["NUMERIC", "DECIMAL", "REAL", "FLOAT", "DOUBLE"],
    "float": ["NUMERIC", "DECIMAL", "REAL", "FLOAT", "DOUBLE"],
    "money": ["NUMERIC", "DECIMAL", "REAL", "FLOAT", "DOUBLE"],
    "bool": ["BOOL", "INT", "NUMERIC"],
    "varchar": ["CHAR", "CLOB", "TEXT"],
    "char": ["CHAR", "CLOB", "TEXT"],
    "text": ["CHAR", "CLOB", "TEXT"],
    "citext": ["CHAR", "CLOB", "TEXT"],
    "timestamp": ["DATE", "TIME", "CHAR", "TEXT", "DATETIME"],
    "time": ["DATE", "TIME", "CHAR", "TEXT", "DATETIME"],
    "date": ["DATE", "TIME", "CHAR", "TEXT", "DATETIME"],
    "uuid": ["CHAR", "TEXT", "UUID", "CLOB"],
    "json": ["JSON", "TEXT", "CHAR", "CLOB"],
    "jsonb": ["JSON", "TEXT", "CHAR", "CLOB"],
}


def _sanitize_cursor_name(s: str) -> str:
    return re.sub(r"[^A-Za-z0-9_]+", "_", s)


def supports_color(force_no: bool) -> bool:
    """Return True when we should emit ANSI colors."""
    if force_no:
        return False
    if os.getenv("NO_COLOR"):
        return False
    term = os.getenv("TERM", "")
    if term == "" or term.lower() == "dumb":
        return False
    try:
        isatty = sys.stdout.isatty()
    except Exception:
        isatty = False
    return isatty


class ColoredFormatter(logging.Formatter):
    LEVEL_COLORS = {
        logging.DEBUG: ANSI["gray"],
        logging.INFO: ANSI["green"],
        logging.WARNING: ANSI["yellow"],
        logging.ERROR: ANSI["red"],
        logging.CRITICAL: ANSI["red"] + ANSI["bold"],
    }

    TAG_COLORS = {
        "SKIP": ANSI["yellow"],
        "SCHEMA": ANSI["blue"],
        "OK": ANSI["magenta"],
    }

    def __init__(self, use_color: bool = True):
        super().__init__(fmt="%(message)s")
        self.use_color = use_color

    def format(self, record: logging.LogRecord) -> str:
        msg = super().format(record)
        parts = msg.split(" ", 1)
        tag = parts[0]
        rest = parts[1] if len(parts) > 1 else ""
        plain_label = f"[{tag}]"

        if not self.use_color:
            return f"{plain_label}{(' ' + rest) if rest else ''}"

        color = self.TAG_COLORS.get(
            tag.upper(), self.LEVEL_COLORS.get(record.levelno, "")
        )
        reset = ANSI["reset"]
        return f"{color}{plain_label}{reset}{(' ' + rest) if rest else ''}"


def setup_logger(verbose: bool, no_color: bool) -> logging.Logger:
    use_color = supports_color(no_color)
    logger = logging.getLogger("db_migrate")
    logger.setLevel(logging.DEBUG if verbose else logging.INFO)
    handler = logging.StreamHandler()
    handler.setLevel(logging.DEBUG if verbose else logging.INFO)
    handler.setFormatter(ColoredFormatter(use_color=use_color))
    logger.handlers.clear()
    logger.addHandler(handler)
    logging.getLogger("psycopg").setLevel(logging.WARNING)

    if verbose:
        logger.debug(f"color_support: {use_color}")
        try:
            isatty = sys.stdout.isatty()
        except Exception:
            isatty = False
        logger.debug(
            f"TERM={os.getenv('TERM', '')!r} "
            f"NO_COLOR={os.getenv('NO_COLOR')!r} "
            f"isatty={isatty}"
        )
    return logger


def quote_sqlite_identifier(name: str) -> str:
    return '"' + name.replace('"', '""') + '"'


def quote_pg_identifier(name: str) -> str:
    """Simple PG identifier quoting for building safe SQL strings."""
    return '"' + name.replace('"', '""') + '"'


def sqlite_decl_satisfies(pg_type: str, sqlite_decl: str) -> bool:
    # Treat empty/blank SQLite declarations more permissively based on PG type.
    decl_raw = sqlite_decl or ""
    if not decl_raw.strip():
        pg = (pg_type or "").lower()
        # arrays -> textual affinity
        if pg.endswith("[]"):
            return True
        # integer-like
        if re.search(r"\b(?:int|serial|bigint)\b", pg):
            return True
        # numeric/float
        if re.search(r"\b(?:numeric|decimal|real|double|float|money)\b", pg):
            return True
        # boolean
        if re.search(r"\b(?:bool|boolean)\b", pg):
            return True
        # binary
        if re.search(r"\b(?:bytea)\b", pg):
            return True
        # textual/json/uuid/timestamps/dates/times
        if re.search(
            r"\b(?:varchar|char|text|citext|jsonb|json|uuid|timestamp|time|date)\b", pg
        ):
            return True
        # conservative fallback: accept empty decl as permissive
        return True

    decl = decl_raw.upper()
    pg = (pg_type or "").lower()

    # array type
    if pg.endswith("[]"):
        return any(tok in decl for tok in ("TEXT", "CHAR", "CLOB", "JSON"))

    for key, allowed_types in TYPE_COMPATIBILITY.items():
        if re.search(r"\b" + re.escape(key) + r"\b", pg):
            return any(tok in decl for tok in allowed_types)

    return any(
        tok in decl for tok in ("TEXT", "CHAR", "CLOB", "NUMERIC", "BLOB", "INT")
    )


# ----------------------
# Postgres helpers
# ----------------------


def list_user_schemas(pg_cursor) -> List[str]:
    pg_cursor.execute(
        """
        SELECT nspname
        FROM pg_namespace
        WHERE nspname NOT LIKE 'pg_%'
          AND nspname != 'information_schema'
          AND nspname != 'public'
        ORDER BY nspname;
        """
    )
    return [r[0] for r in pg_cursor.fetchall()]


def list_tables_in_schema(pg_cursor, schema: str) -> List[str]:
    pg_cursor.execute(
        sql.SQL(
            """
            SELECT table_name
            FROM information_schema.tables
            WHERE table_schema = %s
            ORDER BY table_name;
            """
        ),
        (schema,),
    )
    return [r[0] for r in pg_cursor.fetchall()]


def get_pg_columns(pg_cursor, schema: str, table: str) -> List[Tuple[str, str]]:
    pg_cursor.execute(
        sql.SQL(
            """
            SELECT column_name, data_type
            FROM information_schema.columns
            WHERE table_schema = %s AND table_name = %s
            ORDER BY ordinal_position;
            """
        ),
        (schema, table),
    )
    return [(r[0], r[1]) for r in pg_cursor.fetchall()]


# ----------------------
# SQLite helpers
# ----------------------


class SQLiteCol(NamedTuple):
    cid: int
    name: str
    type: str
    notnull: int
    dflt_value: str
    pk: int


def get_sqlite_table_info(sqlite_cursor, table_name: str) -> list[SQLiteCol]:
    qname = quote_sqlite_identifier(table_name)
    sqlite_cursor.execute(f"PRAGMA table_info({qname});")
    return [SQLiteCol(*row) for row in sqlite_cursor.fetchall()]


def sqlite_table_has_autoincrement(sqlite_cursor, table_name: str) -> bool:
    sqlite_cursor.execute(
        "SELECT sql FROM sqlite_master WHERE type='table' AND name = ?;", (table_name,)
    )
    row = sqlite_cursor.fetchone()
    if not row or not row[0]:
        return False
    return "AUTOINCREMENT" in row[0].upper()


def sqlite_sequence_table_exists(sqlite_cursor) -> bool:
    sqlite_cursor.execute(
        "SELECT name FROM sqlite_master WHERE type='table' AND name='sqlite_sequence';"
    )
    exists = sqlite_cursor.fetchone() is not None
    if not exists:
        logging.getLogger("db_migrate").debug("sqlite_sequence table not present")
    return exists


# ----------------------
# Core migration logic
# ----------------------


def build_select_for_sqlite_columns(
    schema: str,
    table: str,
    sqlite_cols: list[SQLiteCol],
    pg_columns_map: Dict[str, Tuple[str, str]],
) -> str:
    """Return a plain SQL string (no trailing semicolon) selecting PG columns in the order of sqlite_cols.
    Uses simple identifier quoting to avoid psycopg.sql objects which may vary by driver version.
    """
    select_parts = []
    for col in sqlite_cols:
        lc = col.name.lower()
        if lc in pg_columns_map:
            pg_name, pg_type = pg_columns_map[lc]
            pg_type_l = (pg_type or "").lower()
            if any(tok in pg_type_l for tok in ("timestamp", "time")):
                expr = f"{quote_pg_identifier(pg_name)}::text"
            else:
                expr = f"{quote_pg_identifier(pg_name)}"
            select_parts.append(expr)
        else:
            select_parts.append("NULL")
    select_list = ", ".join(select_parts)
    # Quote schema and table names simply (this is not comprehensive for every corner case but avoids psycopg.sql use)
    return f"SELECT {select_list} FROM {quote_pg_identifier(schema)}.{quote_pg_identifier(table)}"


def validate_column_compatibility(
    sqlite_cols: list[SQLiteCol],
    pg_columns_map: Dict[str, Tuple[str, str]],
    schema: str,
    table: str,
) -> None:
    for col in sqlite_cols:
        lc = col.name.lower()
        if lc not in pg_columns_map:
            continue
        pg_type = pg_columns_map[lc][1]
        if not sqlite_decl_satisfies(pg_type, col.type):
            raise ValueError(
                f"Type mismatch for {schema}.{table}.{col.name}: "
                f"PG='{pg_type}' vs SQLite='{col.type}'"
            )


def process_row_for_sqlite(row: Tuple, sqlite_cols: list[SQLiteCol]) -> Tuple:
    out = []
    for i, val in enumerate(row):
        col = sqlite_cols[i]
        decl_raw = col.type or ""
        decl = decl_raw.upper()
        decl_is_empty = decl_raw.strip() == ""

        if isinstance(val, memoryview):
            b = val.tobytes()
            if "BLOB" in decl or decl_is_empty:
                out.append(sqlite3.Binary(b))
            else:
                try:
                    out.append(b.decode("utf-8"))
                except UnicodeDecodeError as e:
                    raise ValueError(
                        f"UTF-8 decode failed for column '{col.name}': {e}"
                    )
        elif isinstance(val, (bytes, bytearray)):
            b = bytes(val)
            if "BLOB" in decl or decl_is_empty:
                out.append(sqlite3.Binary(b))
            else:
                try:
                    out.append(b.decode("utf-8"))
                except UnicodeDecodeError as e:
                    raise ValueError(
                        f"UTF-8 decode failed for column '{col.name}': {e}"
                    )
        else:
            out.append(val)
    return tuple(out)


def fetch_and_validate_row_batches(
    pg_cursor,
    select_sql: str,
    sqlite_cols: list[SQLiteCol],
    batch_size: int = DEFAULT_BATCH_SIZE,
):
    pg_cursor.execute(select_sql)
    total_rows_seen = 0
    while True:
        rows = pg_cursor.fetchmany(batch_size)
        if not rows:
            break
        validated_batch = []
        for row in rows:
            total_rows_seen += 1
            try:
                validated_batch.append(process_row_for_sqlite(row, sqlite_cols))
            except ValueError as e:
                raise ValueError(f"Row {total_rows_seen} validation error: {e}")
        yield validated_batch


def _get_postgres_sequence_info(
    pg_cursor, schema: str, table: str, pk_col: str
) -> Optional[tuple[str, int]]:
    """Get PostgreSQL sequence name and last_value for a table primary key, if any.

    Returns (sequence_name_text, last_value) or None.
    """
    pg_cursor.execute(
        """
        SELECT data_type
        FROM information_schema.columns
        WHERE table_schema = %s AND table_name = %s AND column_name = %s;
        """,
        (schema, table, pk_col),
    )
    r = pg_cursor.fetchone()
    if not r:
        return None

    pg_type = (r[0] or "").lower()
    # Match whole words to avoid matching 'interval' etc.
    if not re.search(r"\b(?:int|serial|bigint)\b", pg_type):
        return None

    # Get sequence name text (NULL if none)
    pg_cursor.execute(
        "SELECT pg_get_serial_sequence(%s, %s);",
        (f"{schema}.{table}", pk_col),
    )
    row = pg_cursor.fetchone()
    seq_name = row[0] if row else None
    if not seq_name:
        return None

    try:
        # Read last_value by casting the pg_get_serial_sequence result to regclass.
        pg_cursor.execute(
            "SELECT last_value FROM pg_get_serial_sequence(%s, %s)::regclass;",
            (f"{schema}.{table}", pk_col),
        )
        rr = pg_cursor.fetchone()
        if rr and rr[0] is not None:
            return (seq_name, int(rr[0]))
    except Exception:
        # Swallow errors (best-effort); callers treat missing info as absent
        pass

    return None


def _get_sqlite_max_pk_value(sqlite_cursor, table: str, pk_col: str) -> Optional[int]:
    """Get the maximum primary key value from SQLite table."""
    logger = logging.getLogger("db_migrate")
    try:
        sqlite_cursor.execute(
            f"SELECT MAX({quote_sqlite_identifier(pk_col)}) "
            f"FROM {quote_sqlite_identifier(table)};"
        )
        r3 = sqlite_cursor.fetchone()
        if r3 and r3[0] is not None:
            return int(r3[0])
    except Exception:
        logger.debug("Failed to read sqlite max pk", exc_info=True)
    return None


def _update_sqlite_sequence(
    sqlite_conn: sqlite3.Connection, table: str, sequence_value: int
) -> bool:
    """Update SQLite sqlite_sequence with new value.

    IMPORTANT: This function does not commit; caller must commit/rollback.
    """
    s_cur = sqlite_conn.cursor()
    s_cur.execute(
        "UPDATE sqlite_sequence SET seq = ? WHERE name = ?;",
        (sequence_value, table),
    )
    if s_cur.rowcount == 0:
        s_cur.execute(
            "INSERT INTO sqlite_sequence(name, seq) VALUES (?, ?);",
            (table, sequence_value),
        )
    return True


def try_update_sqlite_sequence(
    pg_cursor,
    sqlite_conn: sqlite3.Connection,
    schema: str,
    pg_table: str,  # PostgreSQL table name
    sqlite_table: str,  # SQLite table name
    sqlite_cols: list[SQLiteCol],
) -> bool:
    """Update SQLite sequence table based on PostgreSQL sequence values."""
    logger = logging.getLogger("db_migrate")
    # Check if table has a single primary key
    pks = [c for c in sqlite_cols if c.pk]
    if len(pks) != 1:
        return False
    pk_col = pks[0].name

    # Check if SQLite has AUTOINCREMENT (use sqlite_table)
    s_cur = sqlite_conn.cursor()
    if not sqlite_table_has_autoincrement(s_cur, sqlite_table):
        return False
    if not sqlite_sequence_table_exists(s_cur):
        return False

    # Get PostgreSQL sequence info (use pg_table)
    seq_info = _get_postgres_sequence_info(pg_cursor, schema, pg_table, pk_col)
    pg_last_value = seq_info[1] if seq_info else None

    # Get SQLite max PK value (use sqlite_table)
    sqlite_max = _get_sqlite_max_pk_value(s_cur, sqlite_table, pk_col)

    # Determine the value to use
    if pg_last_value is not None and sqlite_max is not None:
        candidate = max(pg_last_value, sqlite_max)
    elif pg_last_value is not None:
        candidate = pg_last_value
    elif sqlite_max is not None:
        candidate = sqlite_max
    else:
        return False

    updated = _update_sqlite_sequence(sqlite_conn, sqlite_table, candidate)
    if updated:
        try:
            sqlite_conn.commit()
        except Exception:
            logger.debug("Failed to commit sqlite_sequence update", exc_info=True)
            # Let caller proceed; treat as best-effort
    return updated


# ----------------------
# Flow: per-schema migration
# ----------------------


def migrate_schema(
    pg_conn,
    sqlite_dir: str,
    schema: str,
    skipped_tables: Set[str],
    logger: logging.Logger,
    dry_run: bool = False,
    batch_size: int = DEFAULT_BATCH_SIZE,
) -> Tuple[int, int]:
    """Migrate a single schema. Returns (tables_migrated, rows_inserted_total)."""
    processed_schema = schema[:-7] if schema.endswith("_schema") else schema
    sqlite_db_path = Path(sqlite_dir) / f"{processed_schema}.db"
    if not sqlite_db_path.is_file():
        logger.error(f"Missing SQLite DB: {sqlite_db_path}")
        return 0, 0

    tables_migrated = 0
    rows_inserted = 0

    sqlite_path = sqlite_db_path.resolve()
    if dry_run:
        uri = sqlite_path.as_uri() + "?mode=ro"
        conn_args = {"database": uri, "uri": True}
    else:
        conn_args = {"database": str(sqlite_path)}

    with sqlite3.connect(**conn_args) as sqlite_conn:
        sqlite_cur = sqlite_conn.cursor()
        sqlite_cur.execute("SELECT name FROM sqlite_master WHERE type='table';")
        sqlite_tables = [r[0] for r in sqlite_cur.fetchall()]
        # O(1) lookup map for matching by lowercase name
        sqlite_table_map = {t.lower(): t for t in sqlite_tables}

        with pg_conn.cursor() as pg_cur:
            # Ensure we start in a clean state for this connection
            try:
                pg_conn.rollback()
            except Exception:
                # Ignore rollback failure; connection is newly opened most likely
                logger.debug(
                    "pg_conn.rollback() at schema start ignored", exc_info=True
                )

            # Get table list for this schema, but catch/rollback on failure
            try:
                tables = list_tables_in_schema(pg_cur, schema)
            except Exception as e:
                logger.error("ERROR %s  (list tables): %s", schema, e)
                logger.debug("Traceback (list tables):", exc_info=True)
                try:
                    pg_conn.rollback()
                except Exception:
                    logger.debug(
                        "pg_conn.rollback() failed after list tables error",
                        exc_info=True,
                    )
                return 0, 0

            for table in tables:
                # Defensive: ensure connection is in a clean state before any new PG work.
                # A prior error can leave the connection in an aborted transaction; calling
                # rollback() clears that and allows subsequent SELECTs to run.
                try:
                    pg_conn.rollback()
                except Exception:
                    # ignore: best-effort cleanup
                    logger.debug(
                        "pg_conn.rollback() ignored at start of table loop",
                        exc_info=True,
                    )

                if table.lower() in skipped_tables:
                    logger.info("SKIP %s.%s  (explicit)", schema, table)
                    continue
                if table.lower() not in sqlite_table_map:
                    logger.info("SKIP %s.%s  (no target table)", schema, table)
                    continue

                matched_table = sqlite_table_map[table.lower()]
                logger.info("OK   %s.%s -> %s", schema, table, matched_table)

                # Fetch PG columns, but defend against aborted transaction here
                try:
                    pg_cols = get_pg_columns(pg_cur, schema, table)
                except Exception as e:
                    logger.error("ERROR %s.%s  (get columns): %s", schema, table, e)
                    logger.debug("Traceback (get columns):", exc_info=True)
                    try:
                        pg_conn.rollback()
                    except Exception:
                        logger.debug(
                            "pg_conn.rollback() failed after get columns error",
                            exc_info=True,
                        )
                    continue

                pg_map: Dict[str, Tuple[str, str]] = {
                    name.lower(): (name, dtype) for name, dtype in pg_cols
                }

                sqlite_info = get_sqlite_table_info(sqlite_cur, matched_table)
                if not sqlite_info:
                    logger.warning(f"SKIP {schema}.{table}  (no sqlite info)")
                    continue

                # Warn once about schema drift (extra columns)
                pg_col_names = {name.lower() for name, _ in pg_cols}
                extra_in_sqlite = {
                    c.name for c in sqlite_info if c.name.lower() not in pg_col_names
                }
                if extra_in_sqlite:
                    logger.warning(
                        f"Schema drift: {schema}.{table} has extra SQLite columns {extra_in_sqlite}"
                    )

                try:
                    validate_column_compatibility(sqlite_info, pg_map, schema, table)
                except ValueError as e:
                    logger.error(f"ERROR {schema}.{table}  (type mismatch): {e}")
                    continue

                select_sql = build_select_for_sqlite_columns(
                    schema, table, sqlite_info, pg_map
                )

                csr_name = _sanitize_cursor_name(f"csr_{schema}_{table}")
                try:
                    with pg_conn.cursor(name=csr_name) as data_cur:
                        batch_gen = fetch_and_validate_row_batches(
                            data_cur, select_sql, sqlite_info, batch_size=batch_size
                        )
                        first_batch = next(batch_gen, None)

                        if not first_batch:
                            logger.info(f"SKIP {schema}.{table}  (no rows)")
                            # data_cur will be closed automatically on leaving the 'with'
                            continue

                        if dry_run:
                            count = len(first_batch)
                            for batch in batch_gen:
                                count += len(batch)
                            inserted = 0
                            logger.info(
                                f"DRY  {schema}.{table}  rows_validated={count}"
                            )
                        else:
                            cur = sqlite_conn.cursor()
                            quoted_table = quote_sqlite_identifier(matched_table)
                            col_names = [c.name for c in sqlite_info]
                            quoted_cols = ", ".join(
                                quote_sqlite_identifier(c) for c in col_names
                            )
                            placeholders = ", ".join(["?"] * len(col_names))

                            cur.execute("BEGIN;")
                            try:
                                cur.execute(f"DELETE FROM {quoted_table};")
                                inserted = 0
                                if first_batch:
                                    cur.executemany(
                                        f"INSERT INTO {quoted_table} ({quoted_cols}) VALUES ({placeholders});",
                                        first_batch,
                                    )
                                    inserted += len(first_batch)
                                for batch in batch_gen:
                                    if not batch:
                                        continue
                                    cur.executemany(
                                        f"INSERT INTO {quoted_table} ({quoted_cols}) VALUES ({placeholders});",
                                        batch,
                                    )
                                    inserted += len(batch)
                            except Exception:
                                cur.execute("ROLLBACK;")
                                raise
                            else:
                                cur.execute("COMMIT;")
                except ValueError as e:
                    logger.error("ERROR %s.%s  (row validation): %s", schema, table, e)
                    try:
                        pg_conn.rollback()
                    except Exception:
                        logger.debug(
                            "pg_conn.rollback() failed after row validation error",
                            exc_info=True,
                        )
                    continue
                except Exception as e:
                    logger.error("ERROR %s.%s  (select failed): %s", schema, table, e)
                    logger.debug("Traceback (select failed):", exc_info=True)
                    try:
                        pg_conn.rollback()
                    except Exception:
                        logger.debug(
                            "pg_conn.rollback() failed after select failed",
                            exc_info=True,
                        )
                    continue

                rows_inserted += inserted
                tables_migrated += 1
                logger.info(f"DONE {schema}.{table}  rows={inserted}")

                if not dry_run:
                    try:
                        if try_update_sqlite_sequence(
                            pg_cur,
                            sqlite_conn,
                            schema,
                            table,
                            matched_table,
                            sqlite_info,
                        ):
                            logger.info(
                                "SEQ  %s.%s  sqlite_sequence updated", schema, table
                            )
                    except Exception as e:
                        logger.warning(
                            "SEQ  %s.%s  update failed (ignored): %s", schema, table, e
                        )

    return tables_migrated, rows_inserted


def migrate_data(
    pg_conn_str: str,
    sqlite_dir: str,
    schema_filter: Optional[str],
    dry_run: bool,
    skip_tables: str,
    logger: logging.Logger,
    batch_size: int,
) -> None:
    skipped_tables = {t.strip().lower() for t in skip_tables.split(",") if t.strip()}
    total_tables = 0
    total_rows = 0
    total_errors = 0

    # List schemas once with a short-lived connection
    with psycopg.connect(pg_conn_str) as tmp_conn:
        with tmp_conn.cursor() as cur:
            schemas = list_user_schemas(cur)

    if schema_filter:
        schemas = [s for s in schemas if s == schema_filter]
    if not schemas:
        logger.error("No schemas to process")
        return

    for schema in schemas:
        logger.info("SCHEMA %s", schema)
        if dry_run:
            logger.info("(dry-run) validating only — no writes will be performed")

        # Use a fresh connection per-schema to isolate failures/aborted transactions
        try:
            with psycopg.connect(pg_conn_str) as pg_conn:
                migrated_tables, inserted_rows = migrate_schema(
                    pg_conn,
                    sqlite_dir,
                    schema,
                    skipped_tables,
                    logger,
                    dry_run=dry_run,
                    batch_size=batch_size,
                )
        except Exception as e:
            logger.error("Schema %s failed: %s", schema, e)
            logger.debug("Traceback (schema failure):", exc_info=True)
            total_errors += 1
            continue

        total_tables += migrated_tables
        total_rows += inserted_rows

    logger.info("---")
    logger.info(
        f"SUMMARY: schemas={len(schemas)} "
        f"tables_migrated={total_tables} "
        f"rows_inserted={total_rows} "
        f"errors={total_errors}"
    )


# ----------------------
# CLI
# ----------------------


def parse_args(argv):
    p = argparse.ArgumentParser(
        description="Migrate Postgres data into per-schema SQLite DBs (non-invasive)."
    )
    p.add_argument("pg_conn", help="Postgres connection string")
    p.add_argument(
        "sqlite_dir", help="Directory containing per-schema sqlite .db files"
    )
    p.add_argument(
        "--schema", help="Only migrate this schema (exact match)", default=None
    )
    p.add_argument(
        "--dry-run",
        help="Validate and report only; do not write to sqlite",
        action="store_true",
    )
    p.add_argument("--verbose", help="Verbose logging (debug)", action="store_true")
    p.add_argument("--no-color", help="Disable ANSI color output", action="store_true")
    p.add_argument(
        "--batch-size",
        type=int,
        default=DEFAULT_BATCH_SIZE,
        help="Batch size for data fetching",
    )
    p.add_argument(
        "--skip-tables",
        help="Comma-separated list of tables to skip",
        default="migrations,servers_stats",
    )

    return p.parse_args(argv[1:])


def main(argv):
    args = parse_args(argv)
    logger = setup_logger(args.verbose, args.no_color)

    sqlite_dir_path = Path(args.sqlite_dir)
    if not sqlite_dir_path.is_dir():
        logger.error("SQLite directory does not exist or is not a directory.")
        raise SystemExit(1)

    try:
        migrate_data(
            args.pg_conn,
            str(sqlite_dir_path),
            args.schema,
            args.dry_run,
            args.skip_tables,
            logger,
            args.batch_size,
        )
    except Exception as e:
        logger.error(f"Fatal error: {e}")
        raise SystemExit(2)


if __name__ == "__main__":
    main(sys.argv)
