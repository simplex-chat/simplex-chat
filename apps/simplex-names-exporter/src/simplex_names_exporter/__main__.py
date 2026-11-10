import argparse
import logging
import os
import sys
import time
from collections import Counter
from collections.abc import Iterator

from prometheus_client import Metric, start_http_server
from prometheus_client.core import CounterMetricFamily, GaugeMetricFamily, HistogramMetricFamily
from prometheus_client.registry import CollectorRegistry

from .chain import Chain
from .names import CATEGORIES, Snapshot, snapshot

MAINNET_REGISTRAR = "0x44052f7dc9c1cef20b0319a6b75288988dbb12a1"
MAX_LENGTH = 63
URL_VARIABLE = "SIMPLEX_NAMES_RPC_URL"


class NamesCollector:
    def __init__(self) -> None:
        self.state: tuple[Snapshot, float] | None = None
        self.failures = 0

    def refresh(self, chain: Chain, registrar: str) -> None:
        try:
            self.state = (snapshot(chain, registrar), time.time())
        except Exception as e:
            self.failures += 1
            logging.warning("refresh failed: %s", e)

    def collect(self) -> Iterator[Metric]:
        yield CounterMetricFamily(
            "simplex_names_refresh_failures", "Refreshes that failed", value=self.failures
        )
        if self.state is None:
            return
        (block, names), refreshed = self.state
        counts = Counter((name.level, name.records) for name in names)
        levels = sorted({2} | {name.level for name in names})
        gauge = GaugeMetricFamily(
            "simplex_names",
            "Registered names by level, and by simplex.contact and simplex.channel set",
            labels=["level", "records"],
        )
        histogram = HistogramMetricFamily(
            "simplex_name_length", "Label lengths in characters, by level", labels=["level"]
        )
        for level in levels:
            for records in CATEGORIES:
                gauge.add_metric([str(level), records], counts[level, records])
            lengths = [name.length for name in names if name.level == level]
            buckets = [(f"{b}.0", sum(n <= b for n in lengths)) for b in range(1, MAX_LENGTH + 1)]
            histogram.add_metric([str(level)], buckets + [("+Inf", len(lengths))], sum(lengths))
        yield gauge
        yield histogram
        yield GaugeMetricFamily("simplex_names_block", "Block the names were read at", value=block)
        yield GaugeMetricFamily(
            "simplex_names_refresh_timestamp_seconds", "Time of the last refresh", value=refreshed
        )


def main() -> None:
    parser = argparse.ArgumentParser(description="Prometheus exporter of SimpleX name statistics")
    parser.add_argument("--host", default="127.0.0.1", help="interface to listen on")
    parser.add_argument("--port", type=int, default=9877, help="port of /metrics")
    parser.add_argument("--interval", type=float, default=300, help="seconds between refreshes")
    parser.add_argument("--registrar", default=MAINNET_REGISTRAR, help="base registrar address")
    args = parser.parse_args()
    url = os.environ.get(URL_VARIABLE)
    if not url:
        sys.exit(f"{URL_VARIABLE} must be the JSON-RPC URL of an Ethereum node")
    logging.basicConfig(level=logging.INFO, format="%(asctime)s %(levelname)s %(message)s")
    collector = NamesCollector()
    registry = CollectorRegistry()
    registry.register(collector)
    start_http_server(args.port, args.host, registry)
    chain = Chain(url)
    while True:
        collector.refresh(chain, args.registrar.lower())
        time.sleep(args.interval)


if __name__ == "__main__":
    main()
