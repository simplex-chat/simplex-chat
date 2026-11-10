# simplex-names-exporter

A Prometheus exporter of the SimpleX names registered on chain: every live name and subname of
every depth, by level, by the `simplex.contact` and `simplex.channel` records set, and a histogram
of their label lengths.

## How names are read

Each refresh reads the chain at one block, in Multicall3 batches of 500 calls:

1. The base registrar's `ens()`, `subnameHook()` (the subname registrar), `baseNode()` and
   `totalSupply()`; the subname registrar's `resolver()`, the default resolver.
2. Every token by `tokenByIndex`, kept while `nameExpires` is after the block's time, with its
   label from `labelOf`. Names in the 90-day grace period are not counted.
3. Each level's children from the subname registrar's `getChildren`, kept while `ownerOf` is not
   zero, which also drops the subnames of a lapsed name or of a previous owner.
4. For each name, `resolver(node)` on the registry, then `text(node, "simplex.contact")` and
   `text(node, "simplex.channel")` on the default resolver. A name on another resolver is counted as
   `other_resolver`, and its records are not read.

## Running

```bash
pip install apps/simplex-names-exporter
SIMPLEX_NAMES_RPC_URL=https://eth-mainnet.g.alchemy.com/v2/<key> simplex-names-exporter --port 9877
```

The RPC URL is read from the environment, so a key in it stays out of `ps`.

| Option | Default | |
| --- | --- | --- |
| `--host` | `127.0.0.1` | Interface of `/metrics`. |
| `--port` | `9877` | Port of `/metrics`. |
| `--interval` | `300` | Seconds between refreshes. |
| `--registrar` | `0x4405…12a1` | The `.simplex` base registrar on mainnet. |

Scrapes are answered from the last refresh. A failed refresh is logged and counted, and the
previous values stay.

## Metrics

| Metric | Type | Labels |
| --- | --- | --- |
| `simplex_names` | gauge | `level`: 2 for `alice.simplex`, 3 for `shop.alice.simplex`, …<br>`records`: `none`, `contact`, `channel`, `both`, `other_resolver` |
| `simplex_name_length` | histogram, a bucket per length 1–63 | `level` |
| `simplex_names_block` | gauge | |
| `simplex_names_refresh_timestamp_seconds` | gauge | |
| `simplex_names_refresh_failures_total` | counter | |

Lengths are in characters (Unicode code points), as the price oracle counts them.

## Tests

```bash
cd apps/simplex-names-exporter
python3 -m pip install -e '.[dev]'
python3 -m pytest -q
```

The tests run against a fake JSON-RPC node that answers each Multicall3 batch from a model of the
contracts.
