"""Incoming business chats: add the roster, log the invite."""

from __future__ import annotations

import logging

from simplex_chat import ChatError
from simplex_chat.types import CEvt, T

from . import messages, roster
from .context import BotContext
from .text import safe_name

log = logging.getLogger(__name__)

# Written to a business chat's custom data once its roster pass has run.
ROSTERED = "rostered"


def _rostered(group: T.GroupInfo) -> bool:
    mark = (group.get("customData") or {}).get(roster.NAMESPACE)
    return isinstance(mark, dict) and mark.get(ROSTERED) is True


async def _mark_rostered(ctx: BotContext, group: T.GroupInfo) -> None:
    """Record that this chat has had its roster pass, preserving other keys.

    Without this, startup repair cannot tell a chat a crash left half-finished
    from one that was completed before the roster changed — and would add
    people who joined the roster later to every conversation the bot has ever
    handled.
    """
    existing = (group.get("customData") or {}).get(roster.NAMESPACE)
    mark: dict[str, object] = dict(existing) if isinstance(existing, dict) else {}
    mark[ROSTERED] = True
    await ctx.api.api_merge_group_custom_data(group, roster.NAMESPACE, mark)


async def _mark(ctx: BotContext, group: T.GroupInfo) -> None:
    """Mark the chat, containing the failure: the next start re-derives it."""
    try:
        await _mark_rostered(ctx, group)
    except ChatError:
        log.warning("could not mark business chat %s as rostered", group["groupId"], exc_info=True)


async def _add_missing(
    ctx: BotContext, group_id: int, entries: list[roster.RosterEntry]
) -> tuple[list[str], list[str]]:
    """Add every entry not already in the group. Returns (added, failed)."""
    present = await roster.contact_ids_in_group(ctx.api, group_id)

    added: list[str] = []
    failed: list[str] = []
    for entry in entries:
        if entry.contact_id in present:
            continue
        try:
            # Final role in one call: promoting a pending invitee re-sends the
            # invitation.
            await ctx.api.api_add_member(group_id, entry.contact_id, ctx.config.member_role)
            added.append(entry.name)
        except ChatError:
            log.exception("failed adding %s to business chat %s", entry.name, group_id)
            failed.append(entry.name)
    return added, failed


async def _roster_for_chats(ctx: BotContext) -> list[roster.RosterEntry]:
    """Active roster members who are still in the roster group.

    Revocation is driven by an event, and the core delivers a queued business
    request before a queued departure just as readily as after it, so the mark
    alone would let somebody who has left read a conversation started after they
    went. Membership of the roster group is the access-control boundary, so it
    is what decides: read for each incoming chat, and once per startup pass.
    """
    entries = await roster.active(ctx.api, ctx.user_id)
    if not entries:
        return []
    present = await roster.contact_ids_in_group(ctx.api, ctx.roster_group_id)
    return [e for e in entries if e.contact_id in present]


async def reconcile_chats(ctx: BotContext, groups: list[T.GroupInfo] | None = None) -> None:
    """Add active roster members to business chats that are missing them.

    Adding members is the only step with no second chance: it is driven by an
    event delivered once, so a crash part-way through the loop would leave that
    customer permanently short of the roster.

    Only chats whose roster pass never completed are touched. A chat that was
    finished before someone joined the roster is left alone: `/dm` promises to
    add you to chats "from now on", and back-filling would hand every past
    customer conversation to whoever joined the roster most recently.
    """
    try:
        entries = await _roster_for_chats(ctx)
        if groups is None:
            groups = await ctx.api.api_list_groups(ctx.user_id)
    except ChatError:
        log.warning("could not reconcile business chats on startup", exc_info=True)
        return

    repaired = 0
    for group in groups:
        if "businessChat" not in group or not roster.in_group(group["membership"]):
            continue
        if _rostered(group):
            continue
        group_id = group["groupId"]
        try:
            added, failed = ([], []) if not entries else await _add_missing(ctx, group_id, entries)
        except ChatError:
            log.warning("could not reconcile business chat %s", group_id, exc_info=True)
            continue
        repaired += 1
        log.info("finished the roster pass for business chat %s on startup", group_id)
        # Reported even when nobody had to be added: the chat was left unmarked,
        # so the crash took the roster group's record of that customer with it.
        await ctx.post_to_roster(_report(_customer_of(group), entries, added, failed))
        if added or not failed:
            # Left unmarked means the repair did not finish; the queued event
            # should be allowed to retry it in this session.
            ctx.repaired.add(group_id)
            # Marked even with an empty roster: the pass has run for this chat,
            # and leaving it unmarked would back-fill whoever joins later.
            await _mark(ctx, group)
    if repaired:
        log.info("finished %d business chats left incomplete by a restart", repaired)


def _report(
    customer: str, entries: list[roster.RosterEntry], added: list[str], failed: list[str]
) -> str:
    """The roster group's record of one business chat."""
    if not entries:
        return messages.EMPTY_ROSTER_LOG.format(customer=customer)
    if not added and not failed:
        return messages.NOBODY_NEW_LOG.format(customer=customer)
    return messages.invite_log(customer, added, failed)


def _customer_of(group: T.GroupInfo) -> str:
    return safe_name((group.get("groupProfile") or {}).get("displayName") or "")


async def on_business_request(ctx: BotContext, evt: CEvt.AcceptingBusinessRequest) -> None:
    """Add every active roster member to a new business chat, then log it."""
    group = evt["groupInfo"]
    group_id = group["groupId"]
    if group_id in ctx.repaired:
        # Startup repair already ran for this chat and reported it; the queued
        # event would otherwise log the same customer a second time.
        ctx.repaired.discard(group_id)
        return
    # For a business chat the group's display name is the customer's own
    # profile string, which the core does not sanitise.
    customer = _customer_of(group)

    # A failure before anything is added must still reach the roster group,
    # which is the operator's only visibility.
    try:
        entries = await _roster_for_chats(ctx)
        added, failed = ([], []) if not entries else await _add_missing(ctx, group_id, entries)
    except ChatError:
        log.exception("failed reading roster for business chat %s", group_id)
        await ctx.post_to_roster(messages.BUSINESS_FAILED_LOG.format(customer=customer))
        return

    await ctx.post_to_roster(_report(customer, entries, added, failed))

    # Marked even when the line above failed to send. The marker records that
    # the pass ran, and an unmarked chat is repaired by every later start with
    # the roster of the day — so withholding it to preserve one log line would
    # hand a past customer's conversation to whoever joins the roster next.
    # Not marked when every add failed and none succeeded: that chat has no
    # roster at all, so the next start should retry rather than skip it.
    if added or not failed:
        await _mark(ctx, group)
