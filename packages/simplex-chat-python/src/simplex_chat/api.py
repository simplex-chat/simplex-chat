"""Low-level escape-hatch API. Most users go through `Bot` instead."""

from __future__ import annotations

import json
from dataclasses import dataclass
from typing import Any, Literal

from . import _native, core, util
from .core import MigrationConfirmation
from .types import CC, CEvt, CR, T

# Mirrors Node `ConnReqType` enum (api.ts:15-18) — the two possible outcomes
# of `api_connect` / `api_connect_active_user` depending on the link kind.
ConnReqType = Literal["invitation", "contact"]


@dataclass(slots=True)
class SqliteDb:
    file_prefix: str
    encryption_key: str | None = None


@dataclass(slots=True)
class PostgresDb:
    connection_string: str
    schema_prefix: str | None = None


Db = SqliteDb | PostgresDb


def _db_to_migrate_args(db: Db) -> tuple[str, str, _native.Backend]:
    """Returns (path-or-prefix, key-or-conn, backend)."""
    if isinstance(db, SqliteDb):
        return (db.file_prefix, db.encryption_key or "", "sqlite")
    if isinstance(db, PostgresDb):
        return (db.schema_prefix or "", db.connection_string, "postgres")
    raise TypeError(f"Unknown db: {db!r}")


class ChatCommandError(Exception):
    """A chat command returned an unexpected response type.

    `response` is the raw wire response; `response_type` exposes its `type`
    discriminator for quick checks. Subclasses cover known recoverable cases
    so callers can `except ContactAlreadyExistsError` instead of inspecting
    `response.get("type")` themselves.
    """

    def __init__(self, message: str, response: CR.ChatResponse):
        super().__init__(message)
        self.response = response

    @property
    def response_type(self) -> str:
        return self.response.get("type", "")  # type: ignore[return-value]


class ContactAlreadyExistsError(ChatCommandError):
    """`api_connect`/`api_connect_active_user` was called for an existing contact."""


class ChatApi:
    def __init__(self, ctrl: int):
        self._ctrl: int | None = ctrl
        self._started = False

    @classmethod
    async def init(
        cls,
        db: Db,
        confirm: MigrationConfirmation = MigrationConfirmation.YES_UP,
    ) -> "ChatApi":
        path_or_prefix, key_or_conn, backend = _db_to_migrate_args(db)
        # Trigger lazy lib load with the right backend BEFORE chat_migrate_init.
        _native.lib_for(backend)
        ctrl = await core.chat_migrate_init(path_or_prefix, key_or_conn, confirm)
        return cls(ctrl)

    @property
    def ctrl(self) -> int:
        """Opaque controller pointer. Raises if `close()` has been called."""
        if self._ctrl is None:
            raise RuntimeError("ChatApi controller not initialized (close() called?)")
        return self._ctrl

    @property
    def initialized(self) -> bool:
        """True until `close()` is called. Mirrors Node `ChatApi.initialized`."""
        return self._ctrl is not None

    @property
    def started(self) -> bool:
        """True between `start_chat()` and the next `stop_chat()` / `close()`."""
        return self._started

    async def start_chat(self) -> None:
        r = await self.send_chat_cmd(
            CC.StartChat_cmd_string({"mainApp": True, "enableSndFiles": True})
        )
        if r.get("type") not in ("chatStarted", "chatRunning"):
            raise ChatCommandError("error starting chat", r)
        self._started = True

    async def stop_chat(self) -> None:
        r = await self.send_chat_cmd("/_stop")
        if r.get("type") != "chatStopped":
            raise ChatCommandError("error stopping chat", r)
        self._started = False

    async def close(self) -> None:
        await core.chat_close_store(self.ctrl)
        self._ctrl = None
        self._started = False

    async def send_chat_cmd(self, cmd: str) -> CR.ChatResponse:
        return await core.chat_send_cmd(self.ctrl, cmd)

    async def recv_chat_event(self, wait_us: int = 500_000) -> CEvt.ChatEvent | None:
        return await core.chat_recv_msg_wait(self.ctrl, wait_us)

    # ------------------------------------------------------------------ #
    # Address commands
    # ------------------------------------------------------------------ #

    async def api_create_user_address(self, user_id: int) -> T.CreatedConnLink:
        r = await self.send_chat_cmd(CC.APICreateMyAddress_cmd_string({"userId": user_id}))
        if r["type"] == "userContactLinkCreated":
            return r["connLinkContact"]
        raise ChatCommandError("error creating user address", r)

    async def api_delete_user_address(self, user_id: int) -> None:
        r = await self.send_chat_cmd(CC.APIDeleteMyAddress_cmd_string({"userId": user_id}))
        if r["type"] != "userContactLinkDeleted":
            raise ChatCommandError("error deleting user address", r)

    async def api_get_user_address(self, user_id: int) -> T.UserContactLink | None:
        try:
            r = await self.send_chat_cmd(CC.APIShowMyAddress_cmd_string({"userId": user_id}))
            if r["type"] == "userContactLink":
                return r["contactLink"]
            raise ChatCommandError("error loading user address", r)
        except core.ChatAPIError as e:
            ce = e.chat_error
            if (
                ce is not None
                and ce.get("type") == "errorStore"
                and ce.get("storeError", {}).get("type") == "userContactLinkNotFound"
            ):
                return None
            raise

    async def api_set_profile_address(
        self, user_id: int, enable: bool
    ) -> T.UserProfileUpdateSummary:
        r = await self.send_chat_cmd(
            CC.APISetProfileAddress_cmd_string({"userId": user_id, "enable": enable})
        )
        if r["type"] == "userProfileUpdated":
            return r["updateSummary"]
        raise ChatCommandError("error setting profile address", r)

    async def api_set_address_settings(self, user_id: int, settings: T.AddressSettings) -> None:
        r = await self.send_chat_cmd(
            CC.APISetAddressSettings_cmd_string({"userId": user_id, "settings": settings})
        )
        if r["type"] != "userContactLinkUpdated":
            raise ChatCommandError("error changing user contact address settings", r)

    # ------------------------------------------------------------------ #
    # Message commands
    # ------------------------------------------------------------------ #

    async def api_send_messages(
        self,
        chat: list | T.ChatRef | T.ChatInfo,
        messages: list[T.ComposedMessage],
        live_message: bool = False,
    ) -> list[T.AChatItem]:
        if isinstance(chat, list):
            send_ref: T.ChatRef = {"chatType": chat[0], "chatId": chat[1]}
        elif "chatType" in chat and "chatId" in chat:
            send_ref = chat
        else:
            ref = util.chat_info_ref(chat)
            if ref is None:
                raise ValueError("api_send_messages: can't send messages to this chat")
            send_ref = ref
        r = await self.send_chat_cmd(
            CC.APISendMessages_cmd_string(
                {
                    "sendRef": send_ref,
                    "composedMessages": messages,
                    "liveMessage": live_message,
                }
            )
        )
        if r["type"] == "newChatItems":
            return r["chatItems"]
        raise ChatCommandError("unexpected response", r)

    async def api_send_text_message(
        self,
        chat: list | T.ChatRef | T.ChatInfo,
        text: str,
        in_reply_to: int | None = None,
    ) -> list[T.AChatItem]:
        msg: T.ComposedMessage = {"msgContent": {"type": "text", "text": text}, "mentions": {}}
        if in_reply_to is not None:
            msg["quotedItemId"] = in_reply_to
        return await self.api_send_messages(chat, [msg])

    async def api_send_text_reply(self, chat_item: T.AChatItem, text: str) -> list[T.AChatItem]:
        return await self.api_send_text_message(
            chat_item["chatInfo"], text, chat_item["chatItem"]["meta"]["itemId"]
        )

    async def api_update_chat_item(
        self,
        chat_type: T.ChatType,
        chat_id: int,
        chat_item_id: int,
        msg_content: T.MsgContent,
        live_message: bool = False,
    ) -> T.ChatItem:
        r = await self.send_chat_cmd(
            CC.APIUpdateChatItem_cmd_string(
                {
                    "chatRef": {"chatType": chat_type, "chatId": chat_id},
                    "chatItemId": chat_item_id,
                    "liveMessage": live_message,
                    "updatedMessage": {"msgContent": msg_content, "mentions": {}},
                }
            )
        )
        if r["type"] == "chatItemUpdated":
            return r["chatItem"]["chatItem"]
        raise ChatCommandError("error updating chat item", r)

    async def api_delete_chat_items(
        self,
        chat_type: T.ChatType,
        chat_id: int,
        chat_item_ids: list[int],
        delete_mode: T.CIDeleteMode,
    ) -> list[T.ChatItemDeletion]:
        r = await self.send_chat_cmd(
            CC.APIDeleteChatItem_cmd_string(
                {
                    "chatRef": {"chatType": chat_type, "chatId": chat_id},
                    "chatItemIds": chat_item_ids,
                    "deleteMode": delete_mode,
                }
            )
        )
        if r["type"] == "chatItemsDeleted":
            return r["chatItemDeletions"]
        raise ChatCommandError("error deleting chat item", r)

    async def api_delete_member_chat_item(
        self, group_id: int, chat_item_ids: list[int]
    ) -> list[T.ChatItemDeletion]:
        r = await self.send_chat_cmd(
            CC.APIDeleteMemberChatItem_cmd_string(
                {"groupId": group_id, "chatItemIds": chat_item_ids}
            )
        )
        if r["type"] == "chatItemsDeleted":
            return r["chatItemDeletions"]
        raise ChatCommandError("error deleting member chat item", r)

    async def api_chat_item_reaction(
        self,
        chat_type: T.ChatType,
        chat_id: int,
        chat_item_id: int,
        add: bool,
        reaction: T.MsgReaction,
    ) -> T.ACIReaction:
        r = await self.send_chat_cmd(
            CC.APIChatItemReaction_cmd_string(
                {
                    "chatRef": {"chatType": chat_type, "chatId": chat_id},
                    "chatItemId": chat_item_id,
                    "add": add,
                    "reaction": reaction,
                }
            )
        )
        if r["type"] == "chatItemReaction":
            return r["reaction"]
        raise ChatCommandError("error setting item reaction", r)

    # ------------------------------------------------------------------ #
    # File commands
    # ------------------------------------------------------------------ #

    async def api_receive_file(self, file_id: int) -> T.AChatItem:
        r = await self.send_chat_cmd(
            CC.ReceiveFile_cmd_string({"fileId": file_id, "userApprovedRelays": True})
        )
        if r["type"] == "rcvFileAccepted":
            return r["chatItem"]
        raise ChatCommandError("error receiving file", r)

    async def api_cancel_file(self, file_id: int) -> None:
        r = await self.send_chat_cmd(CC.CancelFile_cmd_string({"fileId": file_id}))
        if r["type"] not in ("sndFileCancelled", "rcvFileCancelled"):
            raise ChatCommandError("error canceling file", r)

    # ------------------------------------------------------------------ #
    # Group commands
    # ------------------------------------------------------------------ #

    async def api_add_member(
        self, group_id: int, contact_id: int, member_role: T.GroupMemberRole
    ) -> T.GroupMember:
        r = await self.send_chat_cmd(
            CC.APIAddMember_cmd_string(
                {"groupId": group_id, "contactId": contact_id, "memberRole": member_role}
            )
        )
        if r["type"] == "sentGroupInvitation":
            return r["member"]
        raise ChatCommandError("error adding member", r)

    async def api_join_group(self, group_id: int) -> T.GroupInfo:
        r = await self.send_chat_cmd(CC.APIJoinGroup_cmd_string({"groupId": group_id}))
        if r["type"] == "userAcceptedGroupSent":
            return r["groupInfo"]
        raise ChatCommandError("error joining group", r)

    async def api_accept_member(
        self, group_id: int, group_member_id: int, member_role: T.GroupMemberRole
    ) -> T.GroupMember:
        r = await self.send_chat_cmd(
            CC.APIAcceptMember_cmd_string(
                {"groupId": group_id, "groupMemberId": group_member_id, "memberRole": member_role}
            )
        )
        if r["type"] == "memberAccepted":
            return r["member"]
        raise ChatCommandError("error accepting member", r)

    async def api_set_members_role(
        self, group_id: int, group_member_ids: list[int], member_role: T.GroupMemberRole
    ) -> None:
        r = await self.send_chat_cmd(
            CC.APIMembersRole_cmd_string(
                {"groupId": group_id, "groupMemberIds": group_member_ids, "memberRole": member_role}
            )
        )
        if r["type"] != "membersRoleUser":
            raise ChatCommandError("error setting members role", r)

    async def api_block_members_for_all(
        self, group_id: int, group_member_ids: list[int], blocked: bool
    ) -> None:
        r = await self.send_chat_cmd(
            CC.APIBlockMembersForAll_cmd_string(
                {"groupId": group_id, "groupMemberIds": group_member_ids, "blocked": blocked}
            )
        )
        if r["type"] != "membersBlockedForAllUser":
            raise ChatCommandError("error blocking members", r)

    async def api_remove_members(
        self, group_id: int, member_ids: list[int], with_messages: bool = False
    ) -> list[T.GroupMember]:
        r = await self.send_chat_cmd(
            CC.APIRemoveMembers_cmd_string(
                {"groupId": group_id, "groupMemberIds": member_ids, "withMessages": with_messages}
            )
        )
        if r["type"] == "userDeletedMembers":
            return r["members"]
        raise ChatCommandError("error removing member", r)

    async def api_leave_group(self, group_id: int) -> T.GroupInfo:
        r = await self.send_chat_cmd(CC.APILeaveGroup_cmd_string({"groupId": group_id}))
        if r["type"] == "leftMemberUser":
            return r["groupInfo"]
        raise ChatCommandError("error leaving group", r)

    async def api_list_members(self, group_id: int) -> list[T.GroupMember]:
        r = await self.send_chat_cmd(CC.APIListMembers_cmd_string({"groupId": group_id}))
        if r["type"] == "groupMembers":
            return r["group"]["members"]
        raise ChatCommandError("error getting group members", r)

    async def api_new_group(self, user_id: int, group_profile: T.GroupProfile) -> T.GroupInfo:
        r = await self.send_chat_cmd(
            CC.APINewGroup_cmd_string(
                {"userId": user_id, "groupProfile": group_profile, "incognito": False}
            )
        )
        if r["type"] == "groupCreated":
            return r["groupInfo"]
        raise ChatCommandError("error creating group", r)

    async def api_update_group_profile(
        self, group_id: int, group_profile: T.GroupProfile
    ) -> T.GroupInfo:
        r = await self.send_chat_cmd(
            CC.APIUpdateGroupProfile_cmd_string(
                {"groupId": group_id, "groupProfile": group_profile}
            )
        )
        if r["type"] == "groupUpdated":
            return r["toGroup"]
        raise ChatCommandError("error updating group", r)

    # ------------------------------------------------------------------ #
    # Group link commands
    # ------------------------------------------------------------------ #

    async def api_create_group_link(self, group_id: int, member_role: T.GroupMemberRole) -> str:
        r = await self.send_chat_cmd(
            CC.APICreateGroupLink_cmd_string({"groupId": group_id, "memberRole": member_role})
        )
        if r["type"] == "groupLinkCreated":
            link = r["groupLink"]["connLinkContact"]
            return link.get("connShortLink") or link["connFullLink"]
        raise ChatCommandError("error creating group link", r)

    async def api_set_group_link_member_role(
        self, group_id: int, member_role: T.GroupMemberRole
    ) -> None:
        r = await self.send_chat_cmd(
            CC.APIGroupLinkMemberRole_cmd_string({"groupId": group_id, "memberRole": member_role})
        )
        if r["type"] != "groupLink":
            raise ChatCommandError("error setting group link member role", r)

    async def api_delete_group_link(self, group_id: int) -> None:
        r = await self.send_chat_cmd(CC.APIDeleteGroupLink_cmd_string({"groupId": group_id}))
        if r["type"] != "groupLinkDeleted":
            raise ChatCommandError("error deleting group link", r)

    async def api_get_group_link(self, group_id: int) -> T.GroupLink:
        r = await self.send_chat_cmd(CC.APIGetGroupLink_cmd_string({"groupId": group_id}))
        if r["type"] == "groupLink":
            return r["groupLink"]
        raise ChatCommandError("error getting group link", r)

    async def api_get_group_link_str(self, group_id: int) -> str:
        link = (await self.api_get_group_link(group_id))["connLinkContact"]
        return link.get("connShortLink") or link["connFullLink"]

    # ------------------------------------------------------------------ #
    # Connection commands
    # ------------------------------------------------------------------ #

    async def api_create_link(self, user_id: int) -> str:
        r = await self.send_chat_cmd(
            CC.APIAddContact_cmd_string({"userId": user_id, "incognito": False})
        )
        if r["type"] == "invitation":
            link = r["connLinkInvitation"]
            return link.get("connShortLink") or link["connFullLink"]
        raise ChatCommandError("error creating link", r)

    async def api_connect_plan(
        self, user_id: int, connection_link: str
    ) -> tuple[T.ConnectionPlan, T.CreatedConnLink]:
        r = await self.send_chat_cmd(
            CC.APIConnectPlan_cmd_string(
                {"userId": user_id, "connectTarget": connection_link, "resolveMode": "unknown"}
            )
        )
        if r["type"] == "connectionPlan":
            return (r["connectionPlan"], r["connLink"])
        raise ChatCommandError("error getting connect plan", r)

    async def api_connect(
        self,
        user_id: int,
        incognito: bool,
        prepared_link: T.CreatedConnLink | None = None,
    ) -> ConnReqType:
        args: CC.APIConnect = {"userId": user_id, "incognito": incognito}
        if prepared_link is not None:
            args["preparedLink_"] = prepared_link
        r = await self.send_chat_cmd(CC.APIConnect_cmd_string(args))
        return self._handle_connect_result(r)

    async def api_connect_active_user(self, conn_link: str) -> ConnReqType:
        r = await self.send_chat_cmd(
            CC.Connect_cmd_string({"incognito": False, "connTarget_": conn_link})
        )
        return self._handle_connect_result(r)

    def _handle_connect_result(self, r: CR.ChatResponse) -> ConnReqType:
        if r["type"] == "sentConfirmation":
            return "invitation"
        if r["type"] == "sentInvitation":
            return "contact"
        if r["type"] == "contactAlreadyExists":
            raise ContactAlreadyExistsError("contact already exists", r)
        raise ChatCommandError("connection error", r)

    async def api_accept_contact_request(self, contact_req_id: int) -> T.Contact:
        r = await self.send_chat_cmd(
            CC.APIAcceptContact_cmd_string({"contactReqId": contact_req_id})
        )
        if r["type"] == "acceptingContactRequest":
            return r["contact"]
        raise ChatCommandError("error accepting contact request", r)

    async def api_reject_contact_request(self, contact_req_id: int) -> None:
        r = await self.send_chat_cmd(
            CC.APIRejectContact_cmd_string({"contactReqId": contact_req_id})
        )
        if r["type"] != "contactRequestRejected":
            raise ChatCommandError("error rejecting contact request", r)

    # ------------------------------------------------------------------ #
    # Chat commands
    # ------------------------------------------------------------------ #

    async def api_list_contacts(self, user_id: int) -> list[T.Contact]:
        r = await self.send_chat_cmd(CC.APIListContacts_cmd_string({"userId": user_id}))
        if r["type"] == "contactsList":
            return r["contacts"]
        raise ChatCommandError("error listing contacts", r)

    async def api_list_groups(
        self,
        user_id: int,
        contact_id: int | None = None,
        search: str | None = None,
    ) -> list[T.GroupInfo]:
        args: CC.APIListGroups = {"userId": user_id}
        if contact_id is not None:
            args["contactId_"] = contact_id
        if search is not None:
            args["search"] = search
        r = await self.send_chat_cmd(CC.APIListGroups_cmd_string(args))
        if r["type"] == "groupsList":
            return r["groups"]
        raise ChatCommandError("error listing groups", r)

    async def api_get_chats(
        self,
        user_id: int,
        pagination: T.PaginationByTime,
        query: T.ChatListQuery | None = None,
        pending_connections: bool = False,
    ) -> list[T.AChat]:
        if query is None:
            query = {"type": "filters", "favorite": False, "unread": False}
        r = await self.send_chat_cmd(
            CC.APIGetChats_cmd_string(
                {
                    "userId": user_id,
                    "pendingConnections": pending_connections,
                    "pagination": pagination,
                    "query": query,
                }
            )
        )
        if r["type"] == "apiChats":
            return r["chats"]
        raise ChatCommandError("error getting chats", r)

    async def api_delete_chat(
        self,
        chat_type: T.ChatType,
        chat_id: int,
        delete_mode: T.ChatDeleteMode | None = None,
    ) -> None:
        if delete_mode is None:
            delete_mode = {"type": "full", "notify": True}
        r = await self.send_chat_cmd(
            CC.APIDeleteChat_cmd_string(
                {
                    "chatRef": {"chatType": chat_type, "chatId": chat_id},
                    "chatDeleteMode": delete_mode,
                }
            )
        )
        if chat_type == "direct" and r["type"] == "contactDeleted":
            return
        if chat_type == "group" and r["type"] == "groupDeletedUser":
            return
        raise ChatCommandError("error deleting chat", r)

    async def api_set_group_custom_data(
        self, group_id: int, custom_data: dict[str, object] | None = None
    ) -> None:
        args: CC.APISetGroupCustomData = {"groupId": group_id}
        if custom_data is not None:
            args["customData"] = custom_data
        r = await self.send_chat_cmd(CC.APISetGroupCustomData_cmd_string(args))
        if r["type"] != "cmdOk":
            raise ChatCommandError("error setting group custom data", r)

    async def api_set_contact_custom_data(
        self, contact_id: int, custom_data: dict[str, object] | None = None
    ) -> None:
        args: CC.APISetContactCustomData = {"contactId": contact_id}
        if custom_data is not None:
            args["customData"] = custom_data
        r = await self.send_chat_cmd(CC.APISetContactCustomData_cmd_string(args))
        if r["type"] != "cmdOk":
            raise ChatCommandError("error setting contact custom data", r)

    async def api_set_auto_accept_member_contacts(self, user_id: int, on_off: bool) -> None:
        r = await self.send_chat_cmd(
            CC.APISetUserAutoAcceptMemberContacts_cmd_string({"userId": user_id, "onOff": on_off})
        )
        if r["type"] != "cmdOk":
            raise ChatCommandError("error setting auto-accept member contacts", r)

    async def api_get_chat(self, chat_type: T.ChatType, chat_id: int, count: int) -> dict[str, Any]:
        ref = T.ChatType_cmd_string(chat_type) + str(chat_id)
        r = await self.send_chat_cmd(f"/_get chat {ref} count={count}")
        if r["type"] == "apiChat":
            return r["chat"]
        raise ChatCommandError("error getting chat", r)

    # ------------------------------------------------------------------ #
    # User profile commands
    # ------------------------------------------------------------------ #

    async def api_get_active_user(self) -> T.User | None:
        try:
            r = await self.send_chat_cmd(CC.ShowActiveUser_cmd_string({}))
            if r["type"] == "activeUser":
                return r["user"]
            raise ChatCommandError("unexpected response", r)
        except core.ChatAPIError as e:
            ce = e.chat_error
            if (
                ce is not None
                and ce.get("type") == "error"
                and ce.get("errorType", {}).get("type") == "noActiveUser"
            ):
                return None
            raise

    async def api_create_active_user(self, profile: T.Profile | None = None) -> T.User:
        new_user: T.NewUser = {"pastTimestamp": False, "userChatRelay": False, "clientService": False}
        if profile is not None:
            new_user["profile"] = profile
        r = await self.send_chat_cmd(CC.CreateActiveUser_cmd_string({"newUser": new_user}))
        if r["type"] == "activeUser":
            return r["user"]
        raise ChatCommandError("unexpected response", r)

    async def api_list_users(self) -> list[T.UserInfo]:
        r = await self.send_chat_cmd(CC.ListUsers_cmd_string({}))
        if r["type"] == "usersList":
            return r["users"]
        raise ChatCommandError("error listing users", r)

    async def api_set_active_user(self, user_id: int, view_pwd: str | None = None) -> T.User:
        args: CC.APISetActiveUser = {"userId": user_id}
        if view_pwd is not None:
            args["viewPwd"] = view_pwd
        r = await self.send_chat_cmd(CC.APISetActiveUser_cmd_string(args))
        if r["type"] == "activeUser":
            return r["user"]
        raise ChatCommandError("error setting active user", r)

    async def api_delete_user(
        self, user_id: int, del_smp_queues: bool, view_pwd: str | None = None
    ) -> None:
        args: CC.APIDeleteUser = {"userId": user_id, "delSMPQueues": del_smp_queues}
        if view_pwd is not None:
            args["viewPwd"] = view_pwd
        r = await self.send_chat_cmd(CC.APIDeleteUser_cmd_string(args))
        if r["type"] != "cmdOk":
            raise ChatCommandError("error deleting user", r)

    async def api_update_profile(
        self, user_id: int, profile: T.Profile
    ) -> T.UserProfileUpdateSummary | None:
        r = await self.send_chat_cmd(
            CC.APIUpdateProfile_cmd_string({"userId": user_id, "profile": profile})
        )
        if r["type"] == "userProfileNoChange":
            return None
        if r["type"] == "userProfileUpdated":
            return r["updateSummary"]
        raise ChatCommandError("error updating profile", r)

    async def api_set_contact_prefs(self, contact_id: int, preferences: T.Preferences) -> None:
        r = await self.send_chat_cmd(
            CC.APISetContactPrefs_cmd_string({"contactId": contact_id, "preferences": preferences})
        )
        if r["type"] != "contactPrefsUpdated":
            raise ChatCommandError("error setting contact prefs", r)

    # ------------------------------------------------------------------ #
    # Member contact commands
    # ------------------------------------------------------------------ #

    async def api_create_member_contact(self, group_id: int, group_member_id: int) -> T.Contact:
        r = await self.send_chat_cmd(f"/_create member contact #{group_id} {group_member_id}")
        if r["type"] == "newMemberContact":
            return r["contact"]
        raise ChatCommandError("error creating member contact", r)

    async def api_send_member_contact_invitation(
        self,
        contact_id: int,
        message: T.MsgContent | str | None = None,
    ) -> T.Contact:
        cmd = f"/_invite member contact @{contact_id}"
        if message is not None:
            if isinstance(message, str):
                cmd += f" text {message}"
            else:
                cmd += f" json {json.dumps(message)}"
        r = await self.send_chat_cmd(cmd)
        if r["type"] == "newMemberContactSentInv":
            return r["contact"]
        raise ChatCommandError("error sending member contact invitation", r)
