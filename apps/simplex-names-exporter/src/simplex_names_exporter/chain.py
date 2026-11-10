import json
import urllib.request
from collections.abc import Sequence
from typing import Any, NamedTuple

from Crypto.Hash import keccak
from eth_abi import decode, encode

MULTICALL3 = "0xca11bde05977b3631167028862be2a173976ca11"
BATCH = 500


class ChainError(Exception):
    pass


class Call(NamedTuple):
    to: str
    signature: str
    args: tuple[Any, ...]
    returns: tuple[str, ...]


def keccak256(data: bytes) -> bytes:
    return keccak.new(digest_bits=256, data=data).digest()


def selector(signature: str) -> bytes:
    return keccak256(signature.encode())[:4]


def calldata(call: Call) -> bytes:
    types = [t for t in call.signature[call.signature.index("(") + 1 : -1].split(",") if t]
    return selector(call.signature) + encode(types, call.args)


class Chain:
    def __init__(self, url: str, timeout: float = 60) -> None:
        self.url = url
        self.timeout = timeout

    def rpc(self, method: str, params: list[Any]) -> Any:
        body = json.dumps({"jsonrpc": "2.0", "id": 1, "method": method, "params": params})
        headers = {"Content-Type": "application/json"}
        request = urllib.request.Request(self.url, body.encode(), headers)
        with urllib.request.urlopen(request, timeout=self.timeout) as response:
            answer = json.load(response)
        if "error" in answer:
            raise ChainError(f"{method}: {answer['error']}")
        return answer["result"]

    def latest(self) -> tuple[int, int]:
        block = self.rpc("eth_getBlockByNumber", ["latest", False])
        return int(block["number"], 16), int(block["timestamp"], 16)

    def read(self, block: int, calls: Sequence[Call]) -> list[tuple[Any, ...]]:
        results: list[tuple[Any, ...]] = []
        for start in range(0, len(calls), BATCH):
            batch = calls[start : start + BATCH]
            data = selector("aggregate3((address,bool,bytes)[])") + encode(
                ["(address,bool,bytes)[]"], [[(c.to, False, calldata(c)) for c in batch]]
            )
            call = {"to": MULTICALL3, "data": "0x" + data.hex()}
            answer = self.rpc("eth_call", [call, hex(block)])
            (returned,) = decode(["(bool,bytes)[]"], bytes.fromhex(answer[2:]))
            results += [decode(c.returns, ret) for c, (_, ret) in zip(batch, returned)]
        return results
