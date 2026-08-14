"""Everything the handlers need, resolved once at startup."""

from __future__ import annotations

import logging
from dataclasses import dataclass, field

from simplex_chat import ChatApi, ChatError

from .config import Config

log = logging.getLogger(__name__)


@dataclass(slots=True)
class BotContext:
    """API handle plus the ids and config resolved during startup."""

    api: ChatApi
    user_id: int
    roster_group_id: int
    config: Config
    # Business chats repaired by the startup pass, so the queued event for the
    # same chat does not report the customer a second time.
    repaired: set[int] = field(default_factory=set)

    async def post_to_roster(self, text: str) -> None:
        """Send a message to the roster group.

        Never raises: this is the operator's visibility channel, and a failure
        to report an event must not also discard the event that caused it.
        """
        try:
            await self.api.api_send_text_message(["group", self.roster_group_id], text)
        except ChatError:
            log.warning("could not post to the roster group: %s", text, exc_info=True)
