"""Command, connection and membership handlers, plus the roster catch-up pass."""

from __future__ import annotations

import functools
import logging
from collections.abc import Awaitable, Callable

from simplex_chat import Message
from simplex_chat.types import T

from . import messages, roster, setup
from .context import BotContext
from .errors import CHAT_ERRORS
from .text import safe_name

log = logging.getLogger(__name__)


def _reply_on_error(fn: Callable[[BotContext, Message], Awaitable[None]]):
    """Turn a failed command into a visible reply instead of silence."""

    @functools.wraps(fn)
    async def wrapper(ctx: BotContext, msg: Message) -> None:
        try:
            await fn(ctx, msg)
        except CHAT_ERRORS:
            log.exception("%s failed", fn.__name__)
            await msg.reply(messages.COMMAND_FAILED)

    return wrapper


async def _contact_of(ctx: BotContext, member: T.GroupMember) -> T.Contact | None:
    """The sender's direct contact, resolved against current state.

    `memberContactId` in the message payload is a snapshot taken when the core
    built the chat item. `api_create_member_contact` sets that column, so two
    commands sent in quick succession both carry the pre-`/dm` value of `None`
    and would otherwise be treated as having no contact at all.
    """
    contact_id = member.get("memberContactId")
    if contact_id is None:
        for m in await ctx.api.api_list_members(ctx.roster_group_id):
            if m["groupMemberId"] == member["groupMemberId"]:
                contact_id = m.get("memberContactId")
                break
    if contact_id is None:
        return None
    return await roster.find_contact(ctx.api, ctx.user_id, contact_id)


def group_sender(msg: Message) -> T.GroupMember | None:
    """The member who sent a group message, or None if it isn't a group receive."""
    chat_dir = msg.chat_item["chatItem"]["chatDir"]
    if chat_dir.get("type") != "groupRcv":
        return None
    return chat_dir.get("groupMember")


@_reply_on_error
async def dm(ctx: BotContext, msg: Message) -> None:
    """Put the sender on the roster, sending a contact request first if needed."""
    member = group_sender(msg)
    if member is None:
        return

    contact = await _contact_of(ctx, member)

    if contact is not None:
        # api_create_member_contact sets memberContactId before the person has
        # accepted, so an existing contact is not necessarily usable.
        entry = roster.entry_of(contact)

        if roster.contact_usable(contact):
            if entry is not None and entry.state == roster.ACTIVE:
                await msg.reply(messages.ALREADY_ACTIVE)
                return
            since = entry.since if entry else roster.utc_now()
            await roster.mark(ctx.api, contact, roster.ACTIVE, since)
            await msg.reply(messages.ADDED)
            # Every other route to active announces; without this the operator's
            # log misses arrivals that took the fast path.
            name = roster.contact_name(contact)
            await ctx.post_to_roster(messages.NOW_ACTIVE.format(name=name))
            return

        if contact.get("contactGroupMemberId") is None:
            if roster.accept_started(contact):
                # Already accepted; the connection is still completing. Marked
                # here too: contact_ready promotes a pending mark and does
                # nothing without one, so ACCEPTING would promise a roster place
                # that never arrives.
                since = entry.since if entry else roster.utc_now()
                await roster.mark(ctx.api, contact, roster.PENDING, since)
                await msg.reply(messages.ACCEPTING)
                return

            if roster.awaiting_accept(contact):
                # They connected to us from the group rather than accepting our
                # request. Accept it and mark them pending; contactConnected
                # then promotes them exactly as it would the other way round.
                # The library exposes no wrapper for this command.
                await ctx.api.send_chat_cmd(f"/_accept member contact @{contact['contactId']}")
                since = entry.since if entry else roster.utc_now()
                await roster.mark(ctx.api, contact, roster.PENDING, since)
                await msg.reply(messages.ACCEPTING)
                return

            if roster.connecting(contact):
                # The core clears contactGroupMemberId when the peer accepts,
                # well before the connection reports ready, so this shape is
                # also a handshake in progress. Reporting it as gone would send
                # the member to CONNECTION_LOST's advice, and connecting
                # directly there tears down the connection that was completing.
                since = entry.since if entry else roster.utc_now()
                await roster.mark(ctx.api, contact, roster.PENDING, since)
                await msg.reply(messages.CONNECTING)
                return

            # The core clears this once a member contact has connected, and
            # api_send_member_contact_invitation requires it, so the handshake
            # cannot be re-driven from this side. The mark is left alone: an
            # active one renders under "Not reachable", which is the truth.
            await msg.reply(messages.CONNECTION_LOST)
            return

        # Reaching here means contactGroupMemberId is still set, which the core
        # clears on connect: the person never completed the handshake, so an
        # active mark is stale.
        if entry is None or entry.state != roster.PENDING:
            since = entry.since if entry else roster.utc_now()
            await roster.mark(ctx.api, contact, roster.PENDING, since)

        if contact.get("contactGrpInvSent"):
            # The core rejects a second invitation; the person has simply not
            # accepted the first one yet.
            await msg.reply(messages.STILL_PENDING)
            return

        # First send failed. api_create_member_contact would raise "member
        # contact already exists", so resend on the existing contact.
        try:
            await ctx.api.api_send_member_contact_invitation(
                contact["contactId"], messages.INVITATION_TEXT
            )
        except CHAT_ERRORS:
            log.warning("invitation resend to contact %s failed", contact["contactId"])
            await msg.reply(messages.INVITATION_FAILED)
            return
        await msg.reply(messages.INVITATION_SENT)
        return

    contact = await ctx.api.api_create_member_contact(ctx.roster_group_id, member["groupMemberId"])
    await roster.mark(ctx.api, contact, roster.PENDING, roster.utc_now())
    new_contact_id = contact["contactId"]
    try:
        await ctx.api.api_send_member_contact_invitation(new_contact_id, messages.INVITATION_TEXT)
    except CHAT_ERRORS:
        log.warning("invitation to contact %s failed to send", new_contact_id)
        await msg.reply(messages.INVITATION_FAILED)
        return
    await msg.reply(messages.INVITATION_SENT)


async def contact_ready(ctx: BotContext, contact_id: int) -> None:
    """Promote a pending contact once its connection is usable.

    Shared by contactConnected and contactSndReady. Re-reads the contact rather
    than trusting the event payload.
    """
    try:
        contact = await roster.find_contact(ctx.api, ctx.user_id, contact_id)
        if contact is None:
            return
        entry = roster.entry_of(contact)
        if entry is None or entry.state != roster.PENDING:
            return
        if not entry.reachable:
            # The event says the connection is up, but the record is what
            # `active()` will consult, so promote only on what it will see.
            return
        await roster.mark(ctx.api, contact, roster.ACTIVE, entry.since)
    except CHAT_ERRORS:
        # Nobody is waiting on a reply here, so without this the failure is a
        # bare traceback from the library and the person is stranded pending.
        log.warning("could not promote contact %s", contact_id, exc_info=True)
        return
    await ctx.post_to_roster(messages.NOW_ACTIVE.format(name=entry.name))


async def reconcile_roster(ctx: BotContext, groups: list[T.GroupInfo] | None = None) -> None:
    """Catch up on what happened while the bot was stopped.

    Both events this compensates for are delivered once and never replayed: an
    acceptance (`contactConnected`) leaves someone stuck pending, and a removal
    from the roster group leaves someone on the roster who should not be.

    `groups` is passed in by startup so the two passes share one listing, which
    is the largest thing startup reads and grows with every customer ever seen.
    """
    try:
        present = await roster.contact_ids_in_group(ctx.api, ctx.roster_group_id)
        contacts = await ctx.api.api_list_contacts(ctx.user_id)
        if groups is None:
            groups = await ctx.api.api_list_groups(ctx.user_id)
    except CHAT_ERRORS:
        # Startup must not fail because the catch-up pass could not run.
        log.warning("could not reconcile the roster on startup", exc_info=True)
        return

    # Revocation deletes the bot's only durable state, so it runs only when the
    # roster group is unambiguous. An empty member list is deliberately NOT a
    # reason to skip: the last member leaving is when revoking matters most.
    marked = sum(1 for g in groups if setup.is_roster_group(g))
    revoke = marked == 1
    if not revoke:
        log.warning("%d groups carry the roster marker; skipping revocation", marked)

    for contact in contacts:
        entry = roster.entry_of(contact)
        if entry is None:
            continue
        try:
            if revoke and entry.contact_id not in present:
                await roster.unmark(ctx.api, contact)
                log.info("removed %s from the roster: no longer in the roster group", entry.name)
                await ctx.post_to_roster(messages.REMOVED_FROM_GROUP.format(name=entry.name))
            elif entry.state == roster.PENDING and entry.reachable:
                await roster.mark(ctx.api, contact, roster.ACTIVE, entry.since)
                log.info("promoted %s on startup: their connection is ready", entry.name)
                await ctx.post_to_roster(messages.NOW_ACTIVE.format(name=entry.name))
        except CHAT_ERRORS:
            # One bad contact must not abandon the rest of the pass.
            log.warning("could not reconcile contact %s", entry.contact_id, exc_info=True)


def _member_name(member: T.GroupMember) -> str:
    return member.get("localDisplayName") or (member.get("memberProfile") or {}).get(
        "displayName", ""
    )


async def member_gone(ctx: BotContext, group_id: int, member: T.GroupMember) -> None:
    """Take someone off the roster when they leave or are removed from the group.

    Membership of the roster group is the access-control boundary, so it has to
    be revocable: without this, someone removed from the group keeps being added
    to every business chat and cannot even run `/leave` to stop it.
    """
    if group_id != ctx.roster_group_id:
        return
    contact_id = member.get("memberContactId")
    if contact_id is None:
        return
    try:
        contact = await roster.find_contact(ctx.api, ctx.user_id, contact_id)
        if contact is None:
            return
        entry = roster.entry_of(contact)
        if entry is None:
            return
        await roster.unmark(ctx.api, contact)
    except CHAT_ERRORS:
        # The only failure in the bot that the roster group would not hear
        # about, and it is the one on the access-control path. Access is not at
        # risk — every add re-reads roster group membership — but the operator
        # is owed the mark still being there until the next start repairs it.
        log.warning("could not take contact %s off the roster", contact_id, exc_info=True)
        await ctx.post_to_roster(
            messages.REVOKE_FAILED.format(name=safe_name(_member_name(member)))
        )
        return
    log.info("removed %s from the roster: no longer in the roster group", entry.name)
    await ctx.post_to_roster(messages.REMOVED_FROM_GROUP.format(name=entry.name))


@_reply_on_error
async def list_roster(ctx: BotContext, msg: Message) -> None:
    """Reply with the roster, active and pending."""
    entries = await roster.load(ctx.api, ctx.user_id)
    await msg.reply(messages.render_roster(entries))


@_reply_on_error
async def leave(ctx: BotContext, msg: Message) -> None:
    """Take the sender off the roster, keeping the direct contact."""
    member = group_sender(msg)
    if member is None:
        return
    contact = await _contact_of(ctx, member)
    if contact is None or roster.entry_of(contact) is None:
        await msg.reply(messages.NOT_ON_ROSTER)
        return
    await roster.unmark(ctx.api, contact)
    await msg.reply(messages.LEFT)


@_reply_on_error
async def help_cmd(ctx: BotContext, msg: Message) -> None:
    """Reply with the help text."""
    await msg.reply(messages.HELP)
