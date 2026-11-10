import json
import threading
from collections.abc import Callable, Iterator
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer
from typing import Any

import pytest
from eth_abi import decode, encode
from prometheus_client.registry import CollectorRegistry

from simplex_names_exporter import chain, names
from simplex_names_exporter.__main__ import NamesCollector
from simplex_names_exporter.chain import Chain, keccak256, selector

REGISTRAR, REGISTRY, SUBNAMES, RESOLVER, OTHER, OWNER = (f"0x{n:040x}" for n in range(1, 7))
ZERO = "0x" + "00" * 20
BASE = keccak256(bytes(32) + keccak256(b"simplex"))
BLOCK, NOW = 1234, 1_800_000_000
AGGREGATE3 = "0x" + selector("aggregate3((address,bool,bytes)[])").hex()


class Node:
    def __init__(self) -> None:
        self.tokens: list[int] = []
        self.expiries: dict[int, int] = {}
        self.labels: dict[int, str] = {}
        self.resolvers: dict[bytes, str] = {}
        self.texts: dict[tuple[bytes, str], str] = {}
        self.children: dict[bytes, list[tuple[bytes, str]]] = {}
        self.owners: dict[bytes, str] = {}
        self.blocks: list[str] = []
        self.failing = False

    def name(
        self, label: str, expiry: int = NOW + 1, resolver: str = RESOLVER, **texts: str
    ) -> bytes:
        token = int.from_bytes(keccak256(label.encode()))
        self.tokens.append(token)
        self.expiries[token] = expiry
        self.labels[token] = label
        return self.records(keccak256(BASE + token.to_bytes(32)), resolver, texts)

    def subname(self, parent: bytes, label: str, owner: str = OWNER, **texts: str) -> bytes:
        labelhash = keccak256(label.encode())
        self.children.setdefault(parent, []).append((labelhash, label))
        node = keccak256(parent + labelhash)
        self.owners[node] = owner
        return self.records(node, RESOLVER, texts)

    def records(self, node: bytes, resolver: str, texts: dict[str, str]) -> bytes:
        self.resolvers[node] = resolver
        for key in ("contact", "channel"):
            self.texts[node, f"simplex.{key}"] = texts.get(key, "")
        return node

    def page(self, node: bytes, start: int, count: int) -> tuple[list[bytes], list[str]]:
        listed = self.children.get(node, [])[start : start + count]
        return [labelhash for labelhash, _ in listed], [label for _, label in listed]

    def contracts(self) -> dict[tuple[str, str], tuple[tuple[str, ...], Callable[..., Any]]]:
        return {
            (REGISTRAR, "ens()"): (("address",), lambda: (REGISTRY,)),
            (REGISTRAR, "subnameHook()"): (("address",), lambda: (SUBNAMES,)),
            (REGISTRAR, "baseNode()"): (("bytes32",), lambda: (BASE,)),
            (REGISTRAR, "totalSupply()"): (("uint256",), lambda: (len(self.tokens),)),
            (REGISTRAR, "tokenByIndex(uint256)"): (("uint256",), lambda n: (self.tokens[n],)),
            (REGISTRAR, "nameExpires(uint256)"): (("uint256",), lambda i: (self.expiries[i],)),
            (REGISTRAR, "labelOf(uint256)"): (("string",), lambda i: (self.labels[i],)),
            (REGISTRY, "resolver(bytes32)"): (("address",), lambda n: (self.resolvers[n],)),
            (SUBNAMES, "resolver()"): (("address",), lambda: (RESOLVER,)),
            (SUBNAMES, "childrenLength(bytes32)"): (
                ("uint256",),
                lambda n: (len(self.children.get(n, [])),),
            ),
            (SUBNAMES, names.CHILDREN): (("bytes32[]", "string[]"), self.page),
            (SUBNAMES, "ownerOf(uint256)"): (
                ("address",),
                lambda n: (self.owners[n.to_bytes(32)],),
            ),
            (RESOLVER, names.TEXT): (("string",), lambda n, key: (self.texts[n, key],)),
        }

    def call(self, to: str, data: bytes) -> bytes:
        [(signature, (returns, answer))] = [
            (s, entry) for (address, s), entry in self.contracts().items()
            if address == to and selector(s) == data[:4]
        ]
        types = [t for t in signature[signature.index("(") + 1 : -1].split(",") if t]
        return encode(list(returns), answer(*decode(types, data[4:])))

    def answer(self, request: dict[str, Any]) -> dict[str, Any]:
        if self.failing:
            return {"error": {"code": -32000, "message": "unavailable"}}
        if request["method"] == "eth_getBlockByNumber":
            return {"result": {"number": hex(BLOCK), "timestamp": hex(NOW)}}
        call, block = request["params"]
        assert (call["to"], call["data"][:10]) == (chain.MULTICALL3, AGGREGATE3)
        self.blocks.append(block)
        (calls,) = decode(["(address,bool,bytes)[]"], bytes.fromhex(call["data"][10:]))
        results = [(True, self.call(to, data)) for to, _, data in calls]
        return {"result": "0x" + encode(["(bool,bytes)[]"], [results]).hex()}


@pytest.fixture
def node() -> Iterator[tuple[Node, Chain]]:
    state = Node()

    class Handler(BaseHTTPRequestHandler):
        def do_POST(self) -> None:
            request = json.loads(self.rfile.read(int(self.headers["Content-Length"])))
            answer = {"jsonrpc": "2.0", "id": request["id"], **state.answer(request)}
            self.send_response(200)
            self.send_header("Content-Type", "application/json")
            self.end_headers()
            self.wfile.write(json.dumps(answer).encode())

        def log_message(self, *_: Any) -> None:
            pass

    server = ThreadingHTTPServer(("127.0.0.1", 0), Handler)
    threading.Thread(target=server.serve_forever, daemon=True).start()
    yield state, Chain(f"http://127.0.0.1:{server.server_port}")
    server.shutdown()


def collecting() -> tuple[NamesCollector, CollectorRegistry]:
    collector = NamesCollector()
    registry = CollectorRegistry()
    registry.register(collector)
    return collector, registry


@pytest.mark.parametrize("batch, page", [(500, 100), (2, 1)])
def test_counts_live_names_of_every_level(
    node: tuple[Node, Chain], monkeypatch: pytest.MonkeyPatch, batch: int, page: int
) -> None:
    state, rpc = node
    bakery = state.name("alicebakery", contact="simplex:/c", channel="simplex:/g")
    state.name("bob123", contact="simplex:/c")
    state.name("carol7", expiry=NOW)
    state.name("davidd", resolver=OTHER)
    state.name("evelyn")
    shop = state.subname(bakery, "shop", channel="simplex:/g")
    state.subname(shop, "eu")
    old = state.subname(bakery, "old", owner=ZERO)
    state.subname(old, "x")
    monkeypatch.setattr(chain, "BATCH", batch)
    monkeypatch.setattr(names, "PAGE", page)
    collector, registry = collecting()
    collector.refresh(rpc, REGISTRAR)

    def value(name: str, **labels: str) -> float | None:
        return registry.get_sample_value(name, labels)

    level2 = {r: value("simplex_names", level="2", records=r) for r in names.CATEGORIES}
    assert level2 == {"none": 1, "contact": 1, "channel": 0, "both": 1, "other_resolver": 1}
    level3 = {r: value("simplex_names", level="3", records=r) for r in names.CATEGORIES}
    assert level3 == {"none": 0, "contact": 0, "channel": 1, "both": 0, "other_resolver": 0}
    assert value("simplex_names", level="4", records="none") == 1
    assert value("simplex_names", level="5", records="none") is None
    bounds = ("5.0", "6.0", "11.0", "+Inf")
    buckets = [value("simplex_name_length_bucket", level="2", le=le) for le in bounds]
    assert buckets == [0, 3, 4, 4]
    assert value("simplex_name_length_sum", level="2") == 29
    assert value("simplex_name_length_count", level="4") == 1
    assert value("simplex_names_block") == BLOCK
    assert value("simplex_names_refresh_failures_total") == 0
    assert set(state.blocks) == {hex(BLOCK)}


def test_keeps_the_last_snapshot_when_a_refresh_fails(node: tuple[Node, Chain]) -> None:
    state, rpc = node
    state.name("alicebakery")
    collector, registry = collecting()
    none = {"level": "2", "records": "none"}
    assert registry.get_sample_value("simplex_names", none) is None
    collector.refresh(rpc, REGISTRAR)
    state.failing = True
    collector.refresh(rpc, REGISTRAR)
    assert registry.get_sample_value("simplex_names_refresh_failures_total") == 1
    assert registry.get_sample_value("simplex_names", none) == 1
