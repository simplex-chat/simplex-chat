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
import datetime
from typing import List, Tuple, Dict, Optional

import psycopg
from psycopg import sql

# ----------------------
# Small utilities
# ----------------------

# datetime adapters to avoid sqlite3 deprecation noise for Python 3.12+
sqlite3.register_adapter(datetime.datetime, lambda v: v.isoformat(sep=" ", timespec="microseconds"))
sqlite3.register_adapter(datetime.date, lambda v: v.isoformat())
sqlite3.register_adapter(datetime.time, lambda v: v.isoformat(timespec="microseconds"))

ANSI = {
    'reset': '\x1b[0m',
    'bold': '\x1b[1m',
    'dim': '\x1b[2m',
    'red': '\x1b[31m',
    'green': '\x1b[32m',
    'yellow': '\x1b[33m',
    'blue': '\x1b[34m',
    'magenta': '\x1b[35m',
    'cyan': '\x1b[36m',
    'gray': '\x1b[90m',
}


def supports_color(force_no: bool) -> bool:
    """Return True when we should emit ANSI colors.

    Rules:
      - honor --no-color (force_no)
      - honor NO_COLOR environment variable (widely respected)
      - require stdout to be a TTY
      - disallow when TERM is empty or 'dumb'
    """
    if force_no:
        return False
    if os.getenv('NO_COLOR'):
        return False
    term = os.getenv('TERM', '')
    if term == '' or term.lower() == 'dumb':
        return False
    try:
        isatty = sys.stdout.isatty()
    except Exception:
        isatty = False
    return isatty


class ColoredFormatter(logging.Formatter):
    # Map logging level to ANSI color (fallback)
    LEVEL_COLORS = {
        logging.DEBUG: ANSI['gray'],
        logging.INFO: ANSI['green'],
        logging.WARNING: ANSI['yellow'],
        logging.ERROR: ANSI['red'],
        logging.CRITICAL: ANSI['red'] + ANSI['bold'],
    }

    # Explicit tag -> color mapping (overrides level color for these labels)
    TAG_COLORS = {
        'SKIP': ANSI['yellow'],
        'SCHEMA': ANSI['blue'],
        'OK': ANSI['magenta'],
    }

    def __init__(self, use_color: bool = True):
        super().__init__(fmt='%(message)s')
        self.use_color = use_color

    def format(self, record: logging.LogRecord) -> str:
        msg = super().format(record)
        # First token is the label/tag (e.g. "SKIP", "OK", "ERROR", "SCHEMA")
        parts = msg.split(' ', 1)
        tag = parts[0]
        rest = parts[1] if len(parts) > 1 else ''
        plain_label = f"[{tag}]"

        if not self.use_color:
            return f"{plain_label}{(' ' + rest) if rest else ''}"

        # Prefer tag-specific color, otherwise fallback to level color
        color = self.TAG_COLORS.get(tag.upper(), self.LEVEL_COLORS.get(record.levelno, ''))
        reset = ANSI['reset']
        # Color only the bracketed label
        return f"{color}{plain_label}{reset}{(' ' + rest) if rest else ''}"


def setup_logger(verbose: bool, no_color: bool) -> logging.Logger:
    """Create logger and initialize color handling.

    To debug detection issues, run with --verbose; this will emit debug lines showing TERM, NO_COLOR and isatty().
    """
    use_color = supports_color(no_color)

    logger = logging.getLogger('db_migrate')
    logger.setLevel(logging.DEBUG if verbose else logging.INFO)
    handler = logging.StreamHandler()
    handler.setLevel(logging.DEBUG if verbose else logging.INFO)
    handler.setFormatter(ColoredFormatter(use_color=use_color))
    logger.handlers = []
    logger.addHandler(handler)
    logging.getLogger('psycopg').setLevel(logging.WARNING)

    # expose detection details when verbose
    if verbose:
        logger.debug(f"color_support: {use_color}")
        try:
            isatty = sys.stdout.isatty()
        except Exception:
            isatty = False
        logger.debug(f"TERM={os.getenv('TERM', '')!r} NO_COLOR={os.getenv('NO_COLOR')!r} isatty={isatty}")

    return logger


def quote_sqlite_identifier(name: str) -> str:
    return '"' + name.replace('"', '""') + '"'


def sqlite_decl_satisfies(pg_type: Optional[str], sqlite_decl: str) -> bool:
    if sqlite_decl is None:
        return False
    decl = (sqlite_decl or '').upper()
    pg = (pg_type or '').lower()
    if decl.strip() == '':
        return True
    if pg == 'bytea':
        return ('BLOB' in decl) or any(tok in decl for tok in ('CHAR', 'CLOB', 'TEXT', 'JSON'))
    if pg in ('smallint', 'integer', 'bigint', 'int2', 'int4', 'int8', 'serial', 'bigserial'):
        return 'INT' in decl or 'NUMERIC' in decl
    if pg in ('numeric', 'decimal', 'real', 'double precision', 'float4', 'float8', 'money'):
        return any(tok in decl for tok in ('NUMERIC', 'DECIMAL', 'REAL', 'FLOAT', 'DOUBLE'))
    if pg in ('boolean', 'bool'):
        return any(tok in decl for tok in ('BOOL', 'INT', 'NUMERIC'))
    if pg in ('character varying', 'character', 'varchar', 'char', 'text', 'citext'):
        return any(tok in decl for tok in ('CHAR', 'CLOB', 'TEXT'))
    if 'timestamp' in pg or 'time' in pg or pg == 'date':
        return any(tok in decl for tok in ('DATE', 'TIME', 'CHAR', 'TEXT', 'DATETIME'))
    if pg == 'uuid':
        return any(tok in decl for tok in ('CHAR', 'TEXT', 'UUID', 'CLOB'))
    if pg in ('json', 'jsonb') or pg.endswith('[]'):
        return any(tok in decl for tok in ('JSON', 'TEXT', 'CHAR', 'CLOB'))
    return any(tok in decl for tok in ('TEXT', 'CHAR', 'CLOB', 'NUMERIC', 'BLOB', 'INT'))


# ----------------------
# Postgres helpers
# ----------------------

def list_user_schemas(pg_cursor) -> List[str]:
    pg_cursor.execute("""
        SELECT schema_name
        FROM information_schema.schemata
        WHERE schema_name NOT LIKE 'pg_%'
          AND schema_name != 'information_schema'
          AND schema_name != 'public';
    """)
    return [r[0] for r in pg_cursor.fetchall()]


def list_tables_in_schema(pg_cursor, schema: str) -> List[str]:
    pg_cursor.execute("""
        SELECT table_name
        FROM information_schema.tables
        WHERE table_schema = %s
        ORDER BY table_name;
    """, (schema,))
    return [r[0] for r in pg_cursor.fetchall()]


def get_pg_columns(pg_cursor, schema: str, table: str) -> List[Tuple[str, str]]:
    pg_cursor.execute("""
        SELECT column_name, data_type
        FROM information_schema.columns
        WHERE table_schema = %s AND table_name = %s
        ORDER BY ordinal_position;
    """, (schema, table))
    return [(r[0], r[1]) for r in pg_cursor.fetchall()]


# ----------------------
# SQLite helpers
# ----------------------
def get_sqlite_table_info(sqlite_cursor, table_name: str) -> List[Tuple]:
    # Quote the identifier with double-quotes to safely handle names with
    # spaces, quotes, or other special characters.
    qname = quote_sqlite_identifier(table_name)
    sqlite_cursor.execute(f"PRAGMA table_info({qname});")
    return sqlite_cursor.fetchall()

def sqlite_table_has_autoincrement(sqlite_cursor, table_name: str) -> bool:
    sqlite_cursor.execute("SELECT sql FROM sqlite_master WHERE type='table' AND name = ?;", (table_name,))
    row = sqlite_cursor.fetchone()
    if not row or not row[0]:
        return False
    return 'AUTOINCREMENT' in row[0].upper()


def sqlite_sequence_table_exists(sqlite_cursor) -> bool:
    sqlite_cursor.execute("SELECT name FROM sqlite_master WHERE type='table' AND name='sqlite_sequence';")
    return sqlite_cursor.fetchone() is not None


# ----------------------
# Core migration logic
# ----------------------

def build_select_for_sqlite_columns(schema: str, table: str,
                                    sqlite_cols: List[Tuple],
                                    pg_columns_map: Dict[str, Tuple[str, str]]) -> sql.Composed:
    select_exprs = []
    for cid, colname, declared_type, notnull, dflt, pk in sqlite_cols:
        lc = colname.lower()
        if lc in pg_columns_map:
            pg_name, pg_type = pg_columns_map[lc]
            if any(tok in (pg_type or '') for tok in ('timestamp', 'time')):
                expr = sql.SQL('{}::text').format(sql.Identifier(pg_name))
            else:
                expr = sql.Identifier(pg_name)
            select_exprs.append(expr)
        else:
            select_exprs.append(sql.SQL('NULL'))
    return sql.SQL('SELECT {} FROM {}.{};').format(sql.SQL(', ').join(select_exprs), sql.Identifier(schema), sql.Identifier(table))


def validate_column_compatibility(sqlite_cols: List[Tuple], pg_columns_map: Dict[str, Tuple[str, str]], schema: str, table: str):
    for cid, colname, declared_type, notnull, dflt, pk in sqlite_cols:
        lc = colname.lower()
        if lc not in pg_columns_map:
            continue
        pg_type = pg_columns_map.get(lc)[1]
        if not sqlite_decl_satisfies(pg_type, declared_type or ''):
            raise ValueError(f"Type mismatch for {schema}.{table}.{colname}: PG='{pg_type}' vs SQLite='{declared_type}'")


def process_row_for_sqlite(row: Tuple, sqlite_cols: List[Tuple]):
    out = []
    for i, val in enumerate(row):
        _, colname, declared_type, _, _, _ = sqlite_cols[i]
        decl_raw = (declared_type or '')
        decl = decl_raw.upper()
        decl_is_empty = decl_raw.strip() == ''

        if isinstance(val, memoryview):
            b = val.tobytes()
            if 'BLOB' in decl or decl_is_empty:
                out.append(sqlite3.Binary(b))
            else:
                try:
                    out.append(b.decode('utf-8'))
                except UnicodeDecodeError as e:
                    raise ValueError(f"UTF-8 decode failed for column '{colname}': {e}")
        elif isinstance(val, (bytes, bytearray)):
            b = bytes(val)
            if 'BLOB' in decl or decl_is_empty:
                out.append(sqlite3.Binary(b))
            else:
                try:
                    out.append(b.decode('utf-8'))
                except UnicodeDecodeError as e:
                    raise ValueError(f"UTF-8 decode failed for column '{colname}': {e}")
        else:
            out.append(val)
    return tuple(out)


def fetch_and_validate_all_rows(pg_cursor, select_sql: sql.Composed, sqlite_cols: List[Tuple]):
    pg_cursor.execute(select_sql)
    rows = pg_cursor.fetchall()
    if not rows:
        return []
    validated = []
    for ridx, row in enumerate(rows, start=1):
        try:
            validated.append(process_row_for_sqlite(row, sqlite_cols))
        except ValueError as e:
            raise ValueError(f"Row {ridx} validation error: {e}")
    return validated


def write_rows_into_sqlite(sqlite_conn: sqlite3.Connection, table_name: str, sqlite_cols: List[Tuple], rows: List[Tuple]):
    if not rows:
        return 0
    cur = sqlite_conn.cursor()
    quoted_table = quote_sqlite_identifier(table_name)
    col_names = [c[1] for c in sqlite_cols]
    quoted_cols = ', '.join(quote_sqlite_identifier(c) for c in col_names)
    placeholders = ', '.join(['?'] * len(col_names))

    cur.execute('BEGIN;')
    try:
        cur.execute(f'DELETE FROM {quoted_table};')
        cur.executemany(f'INSERT INTO {quoted_table} ({quoted_cols}) VALUES ({placeholders});', rows)
    except Exception:
        cur.execute('ROLLBACK;')
        raise
    else:
        cur.execute('COMMIT;')
    return len(rows)


def try_update_sqlite_sequence(pg_cursor, sqlite_conn: sqlite3.Connection, schema: str, table: str, sqlite_cols: List[Tuple]):
    pks = [c for c in sqlite_cols if c[5]]
    if len(pks) != 1:
        return False
    pk_col = pks[0][1]

    s_cur = sqlite_conn.cursor()
    if not sqlite_table_has_autoincrement(s_cur, table):
        return False
    if not sqlite_sequence_table_exists(s_cur):
        return False

    pg_cursor.execute(
        "SELECT data_type FROM information_schema.columns WHERE table_schema = %s AND table_name = %s AND column_name = %s;",
        (schema, table, pk_col),
    )
    r = pg_cursor.fetchone()
    if not r:
        return False
    pg_type = (r[0] or '').lower()
    if not any(tok in pg_type for tok in ('int', 'serial', 'bigint')):
        return False

    pg_cursor.execute('SELECT pg_get_serial_sequence(%s, %s);', (f"{schema}.{table}", pk_col))
    seq_name_row = pg_cursor.fetchone()
    seq_name = seq_name_row[0] if seq_name_row else None
    last_val: Optional[int] = None

    if seq_name:
        seq_short = seq_name.split('.')[-1]
        try:
            pg_cursor.execute('SELECT last_value FROM pg_sequences WHERE schemaname = %s AND sequencename = %s;', (schema, seq_short))
            r2 = pg_cursor.fetchone()
            if r2 and r2[0] is not None:
                last_val = int(r2[0])
        except Exception:
            last_val = None

        if last_val is None:
            try:
                parts = seq_name.split('.')
                if len(parts) == 2:
                    pg_cursor.execute(sql.SQL('SELECT last_value FROM {}.{};').format(sql.Identifier(parts[0]), sql.Identifier(parts[1])))
                else:
                    pg_cursor.execute(sql.SQL('SELECT last_value FROM {};').format(sql.Identifier(seq_name)))
                rr = pg_cursor.fetchone()
                if rr and rr[0] is not None:
                    last_val = int(rr[0])
            except Exception:
                last_val = None

    s_cur.execute(f'SELECT MAX({quote_sqlite_identifier(pk_col)}) FROM {quote_sqlite_identifier(table)};')
    r3 = s_cur.fetchone()
    sqlite_max = None
    if r3:
        try:
            sqlite_max = int(r3[0]) if r3[0] is not None else None
        except Exception:
            sqlite_max = None

    candidate = None
    if last_val is not None and sqlite_max is not None:
        candidate = max(last_val, sqlite_max)
    elif last_val is not None:
        candidate = last_val
    elif sqlite_max is not None:
        candidate = sqlite_max

    if candidate is None:
        return False

    s_cur.execute('UPDATE sqlite_sequence SET seq = ? WHERE name = ?;', (candidate, table))
    if s_cur.rowcount == 0:
        s_cur.execute('INSERT INTO sqlite_sequence(name, seq) VALUES (?, ?);', (table, candidate))
    sqlite_conn.commit()
    return True


# ----------------------
# Flow: per-schema migration
# ----------------------

def migrate_schema(pg_conn, sqlite_dir: str, schema: str, skipped_tables: set, logger: logging.Logger) -> Tuple[int, int]:
    """Migrate a single schema. Returns (tables_migrated, rows_inserted_total)."""
    processed_schema = schema[:-7] if schema.endswith('_schema') else schema
    sqlite_db_path = os.path.join(sqlite_dir, f"{processed_schema}.db")
    if not os.path.exists(sqlite_db_path):
        logger.error(f"Missing SQLite DB: {sqlite_db_path}")
        return 0, 0

    tables_migrated = 0
    rows_inserted = 0

    with sqlite3.connect(sqlite_db_path) as sqlite_conn:
        sqlite_cur = sqlite_conn.cursor()
        sqlite_cur.execute("SELECT name FROM sqlite_master WHERE type='table';")
        sqlite_tables = [r[0] for r in sqlite_cur.fetchall()]
        sqlite_tables_lower = [t.lower() for t in sqlite_tables]

        with pg_conn.cursor() as pg_cur:
            tables = list_tables_in_schema(pg_cur, schema)
            for table in tables:
                if table in skipped_tables:
                    logger.info(f"SKIP {schema}.{table}  (explicit)")
                    continue
                if table.lower() not in sqlite_tables_lower:
                    logger.info(f"SKIP {schema}.{table}  (no target table)")
                    continue

                matched_table = sqlite_tables[sqlite_tables_lower.index(table.lower())]
                logger.info(f"OK   {schema}.{table} -> {matched_table}")

                pg_cols = get_pg_columns(pg_cur, schema, table)
                pg_map: Dict[str, Tuple[str, str]] = {name.lower(): (name, dtype) for name, dtype in pg_cols}

                sqlite_info = get_sqlite_table_info(sqlite_cur, matched_table)
                if not sqlite_info:
                    logger.warning(f"SKIP {schema}.{table}  (no sqlite info)")
                    continue

                try:
                    validate_column_compatibility(sqlite_info, pg_map, schema, table)
                except ValueError as e:
                    logger.error(f"ERROR {schema}.{table}  (type mismatch): {e}")
                    continue

                select_sql = build_select_for_sqlite_columns(schema, table, sqlite_info, pg_map)

                try:
                    validated_rows = fetch_and_validate_all_rows(pg_cur, select_sql, sqlite_info)
                except ValueError as e:
                    logger.error(f"ERROR {schema}.{table}  (row validation): {e}")
                    continue

                if not validated_rows:
                    logger.info(f"SKIP {schema}.{table}  (no rows)")
                    continue

                try:
                    if not logger:  # placeholder to show dry-run intent is unchanged
                        pass
                    inserted = write_rows_into_sqlite(sqlite_conn, matched_table, sqlite_info, validated_rows)
                except Exception as e:
                    logger.error(f"ERROR {schema}.{table}  (write failed): {e}")
                    continue

                rows_inserted += inserted
                tables_migrated += 1
                logger.info(f"DONE {schema}.{table}  rows={inserted}")

                try:
                    if try_update_sqlite_sequence(pg_cur, sqlite_conn, schema, table, sqlite_info):
                        logger.info(f"SEQ  {schema}.{table}  sqlite_sequence updated")
                except Exception as e:
                    logger.warning(f"SEQ  {schema}.{table}  update failed (ignored): {e}")

        sqlite_cur.close()

    return tables_migrated, rows_inserted


def migrate_data(pg_conn_str: str, sqlite_dir: str, schema_filter: Optional[str], dry_run: bool, logger: logging.Logger):
    skipped_tables = {'migrations', 'servers_stats'}
    total_tables = 0
    total_rows = 0
    total_errors = 0

    with psycopg.connect(pg_conn_str) as pg_conn:
        with pg_conn.cursor() as cur:
            schemas = list_user_schemas(cur)
        if schema_filter:
            schemas = [s for s in schemas if s == schema_filter]
        if not schemas:
            logger.error('No schemas to process')
            return

        for schema in schemas:
            logger.info(f"SCHEMA {schema}")
            if dry_run:
                logger.info("(dry-run) validating only — no writes will be performed")

            try:
                migrated_tables, inserted_rows = migrate_schema(pg_conn, sqlite_dir, schema, skipped_tables, logger)
            except Exception as e:
                logger.error(f"Schema {schema} failed: {e}")
                total_errors += 1
                continue

            total_tables += migrated_tables
            total_rows += inserted_rows

    logger.info('---')
    logger.info(f"SUMMARY: schemas={len(schemas)} tables_migrated={total_tables} rows_inserted={total_rows} errors={total_errors}")


# ----------------------
# CLI
# ----------------------


def parse_args(argv):
    p = argparse.ArgumentParser(description='Migrate Postgres data into per-schema SQLite DBs (non-invasive).')
    p.add_argument('pg_conn', help='Postgres connection string')
    p.add_argument('sqlite_dir', help='Directory containing per-schema sqlite .db files')
    p.add_argument('--schema', help='Only migrate this schema (exact match)', default=None)
    p.add_argument('--dry-run', help='Validate and report only; do not write to sqlite', action='store_true')
    p.add_argument('--verbose', help='Verbose logging (debug)', action='store_true')
    p.add_argument('--no-color', help='Disable ANSI color output', action='store_true')
    return p.parse_args(argv[1:])


def main(argv):
    args = parse_args(argv)
    logger = setup_logger(args.verbose, args.no_color)

    if not os.path.isdir(args.sqlite_dir):
        logger.error('SQLite directory does not exist or is not a directory.')
        sys.exit(1)

    try:
        migrate_data(args.pg_conn, args.sqlite_dir, args.schema, args.dry_run, logger)
    except Exception as e:
        logger.error(f'Fatal error: {e}')
        sys.exit(2)


if __name__ == '__main__':
    main(sys.argv)
