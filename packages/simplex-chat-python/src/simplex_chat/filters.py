"""Compile kwarg-based message filters into a single predicate."""

from __future__ import annotations

import re
from typing import Any, Callable


def compile_message_filter(kw: dict[str, Any]) -> Callable[[Any], bool]:
    """Compile filter kwargs into a single predicate function.

    Multiple kwargs combine with AND; tuples within a kwarg combine with OR.
    `when` is the last predicate evaluated.
    """
    predicates: list[Callable[[Any], bool]] = []

    if (ct := kw.get("content_type")) is not None:
        ct_set = (ct,) if isinstance(ct, str) else tuple(ct)
        predicates.append(lambda m: m.content.get("type") in ct_set)

    if (t := kw.get("text")) is not None:
        if isinstance(t, re.Pattern):
            predicates.append(lambda m: bool(t.search(m.content.get("text", "") or "")))
        else:
            predicates.append(lambda m: m.content.get("text") == t)

    if (cht := kw.get("chat_type")) is not None:
        cht_set = (cht,) if isinstance(cht, str) else tuple(cht)
        predicates.append(lambda m: m.chat_item["chatInfo"]["type"] in cht_set)

    if (gid := kw.get("group_id")) is not None:
        gid_set: tuple[int, ...] = (gid,) if isinstance(gid, int) else tuple(gid)

        def gid_match(m: Any) -> bool:
            ci = m.chat_item["chatInfo"]
            return ci["type"] == "group" and ci["groupInfo"]["groupId"] in gid_set

        predicates.append(gid_match)

    if (cid := kw.get("contact_id")) is not None:
        cid_set: tuple[int, ...] = (cid,) if isinstance(cid, int) else tuple(cid)

        def cid_match(m: Any) -> bool:
            ci = m.chat_item["chatInfo"]
            return ci["type"] == "direct" and ci["contact"]["contactId"] in cid_set

        predicates.append(cid_match)

    if (when := kw.get("when")) is not None:
        predicates.append(when)

    if not predicates:
        return lambda _m: True
    return lambda m: all(p(m) for p in predicates)
