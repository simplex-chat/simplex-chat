from collections.abc import Callable, Sequence
from typing import Any, NamedTuple

from .chain import Call, Chain, keccak256

PAGE = 100
KEYS = ("simplex.contact", "simplex.channel")
TEXT = "text(bytes32,string)"
CHILDREN = "getChildren(bytes32,uint256,uint256)"
CATEGORIES = ("none", "contact", "channel", "both", "other_resolver")
FILLED = {
    (False, False): "none",
    (True, False): "contact",
    (False, True): "channel",
    (True, True): "both",
}

Read = Callable[[Sequence[Call]], list[tuple[Any, ...]]]
Node = tuple[bytes, str]


class Name(NamedTuple):
    level: int
    length: int
    records: str


class Snapshot(NamedTuple):
    block: int
    names: list[Name]


def snapshot(chain: Chain, registrar: str) -> Snapshot:
    block, now = chain.latest()

    def read(calls: Sequence[Call]) -> list[tuple[Any, ...]]:
        return chain.read(block, calls)

    [(registry,), (subnames,), (base,), (total,)] = read(
        [
            Call(registrar, "ens()", (), ("address",)),
            Call(registrar, "subnameHook()", (), ("address",)),
            Call(registrar, "baseNode()", (), ("bytes32",)),
            Call(registrar, "totalSupply()", (), ("uint256",)),
        ]
    )
    [(resolver,)] = read([Call(subnames, "resolver()", (), ("address",))])
    indexes = [Call(registrar, "tokenByIndex(uint256)", (n,), ("uint256",)) for n in range(total)]
    ids = [i for (i,) in read(indexes)]
    expiries = read([Call(registrar, "nameExpires(uint256)", (i,), ("uint256",)) for i in ids])
    live = [i for i, (expiry,) in zip(ids, expiries) if expiry > now]
    labels = read([Call(registrar, "labelOf(uint256)", (i,), ("string",)) for i in live])
    nodes = [(keccak256(base + i.to_bytes(32)), label) for i, (label,) in zip(live, labels)]
    names: list[Name] = []
    level = 2
    while nodes:
        records = categories(read, registry, resolver, nodes)
        names += [Name(level, len(label), r) for (_, label), r in zip(nodes, records)]
        nodes = children(read, subnames, nodes)
        level += 1
    return Snapshot(block, names)


def categories(read: Read, registry: str, resolver: str, nodes: list[Node]) -> list[str]:
    resolvers = read([Call(registry, "resolver(bytes32)", (n,), ("address",)) for n, _ in nodes])
    own = [n for (n, _), (r,) in zip(nodes, resolvers) if r == resolver]
    texts = read([Call(resolver, TEXT, (n, key), ("string",)) for n in own for key in KEYS])
    pairs = zip(own, texts[::2], texts[1::2])
    filled = {n: FILLED[contact != "", channel != ""] for n, (contact,), (channel,) in pairs}
    return [filled.get(n, "other_resolver") for n, _ in nodes]


def children(read: Read, subnames: str, parents: list[Node]) -> list[Node]:
    counts = read(
        [Call(subnames, "childrenLength(bytes32)", (n,), ("uint256",)) for n, _ in parents]
    )
    pages = [
        (n, start) for (n, _), (count,) in zip(parents, counts) for start in range(0, count, PAGE)
    ]
    listed = read(
        [
            Call(subnames, CHILDREN, (n, start, PAGE), ("bytes32[]", "string[]"))
            for n, start in pages
        ]
    )
    found = [
        (keccak256(n + labelhash), label)
        for (n, _), (labelhashes, labels) in zip(pages, listed)
        for labelhash, label in zip(labelhashes, labels)
    ]
    owners = read(
        [Call(subnames, "ownerOf(uint256)", (int.from_bytes(n),), ("address",)) for n, _ in found]
    )
    return [child for child, (owner,) in zip(found, owners) if int(owner, 16)]
