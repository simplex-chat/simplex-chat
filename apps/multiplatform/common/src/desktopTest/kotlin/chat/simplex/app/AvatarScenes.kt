package chat.simplex.app

import androidx.compose.foundation.layout.Arrangement
import androidx.compose.foundation.layout.Box
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.Row
import androidx.compose.foundation.layout.padding
import androidx.compose.foundation.rememberScrollState
import androidx.compose.foundation.verticalScroll
import androidx.compose.material.MaterialTheme
import androidx.compose.runtime.Composable
import androidx.compose.runtime.mutableStateOf
import androidx.compose.runtime.remember
import androidx.compose.ui.Modifier
import androidx.compose.ui.unit.dp
import chat.simplex.common.model.*
import chat.simplex.common.ui.theme.SimpleXTheme
import chat.simplex.common.views.badges.BadgeLevel
import chat.simplex.common.views.badges.BadgeUserPreview
import chat.simplex.common.views.call.Call
import chat.simplex.common.views.call.CallMediaType
import chat.simplex.common.views.call.CallState
import chat.simplex.common.views.chat.ChatInfoToolbarTitle
import chat.simplex.common.views.chat.ComposeChatLinkView
import chat.simplex.common.views.chat.group.ChannelMembersView
import chat.simplex.common.views.chat.group.ContactCheckRow
import chat.simplex.common.views.chat.group.MemberRow
import chat.simplex.common.views.chat.group.MemberSupportChatToolbarTitle
import chat.simplex.common.views.chat.group.SupportChatRow
import chat.simplex.common.views.chat.item.CIChatLinkHeader
import chat.simplex.common.views.chat.item.previewDate
import chat.simplex.common.views.chatlist.ActiveCallInteractiveArea
import chat.simplex.common.views.chatlist.ChatListNavLinkView
import chat.simplex.common.views.chatlist.ChatPreviewView
import chat.simplex.common.views.chatlist.ContactConnectionView
import chat.simplex.common.views.chatlist.ContactRequestView
import chat.simplex.common.views.chatlist.ShareListNavLinkView
import chat.simplex.common.views.chatlist.UserPickerUsersSection
import chat.simplex.common.views.chatlist.UserProfileButton
import chat.simplex.common.views.chatlist.UserProfileRow
import chat.simplex.common.views.contacts.ContactPreviewView
import chat.simplex.common.views.helpers.ModalData
import chat.simplex.common.views.usersettings.AppearanceScope
import chat.simplex.common.views.usersettings.ProfilePreview
import java.io.Closeable

private const val PHOTO_ORANGE = "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAEAAAABACAIAAAAlC+aJAAAAkklEQVR42u3YSRGAQAwEwBhEDVZwgSIcIIM/N/tKlq4aAdO/SWIZh9IJAAAAAAAAgOas87RLDcCxd7MkcrZ/z4jM7d8YInn7R0Pkb39viBLtewBcGaJK+x4ApwYAAAAAgN8ATIk/j7nyc7qHg6aHk/KrIfVfqPBb5V7itQgAAAAAAAAAAAAAAAAAAAAAAAAA8JwNNzyvMTR6CG8AAAAASUVORK5CYII="
private const val PHOTO_PURPLE = "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAEAAAABACAIAAAAlC+aJAAAAkklEQVR42u3YOxWAQAwEwNjEEw5wQoMN5NDz56rkmPdWwE63SYzDUjoBAAAAAAAA0Jx5WnepATj2bpZEzvbvGZG5/RtDJG//aIj87e8NUaJ9D4ArQ1Rp3wPg1AAAAAAA8BuAKfHnMVd+Tvdw0PRwUn41pP4LFX6r3Eu8FgEAAAAAAAAAAAAAAAAAAAAAAAAAnrMB8ChVMTESNhwAAAAASUVORK5CYII="

private fun contact(id: Long, name: String, image: String?, peerType: ChatPeerType? = null) =
  Contact.sampleData.copy(contactId = id, localDisplayName = name, profile = LocalProfile.sampleData.copy(displayName = name, fullName = "", image = image, peerType = peerType))

private fun group(id: Long, name: String, image: String?, channel: Boolean = false, business: Boolean = false) =
  GroupInfo.sampleData.copy(
    groupId = id,
    localDisplayName = name,
    groupProfile = GroupInfo.sampleData.groupProfile.copy(displayName = name, fullName = "", image = image),
    useRelays = channel,
    businessChat = if (business) BusinessChatInfo(BusinessChatType.Business, "business", "customer") else null
  )

private fun member(id: Long, name: String, image: String?) =
  GroupMember.sampleData.copy(groupMemberId = id, memberId = "m$id", localDisplayName = name, memberProfile = LocalProfile.sampleData.copy(profileId = id, displayName = name, fullName = "", image = image))

private fun user(id: Long, name: String, image: String?, active: Boolean) =
  User.sampleData.copy(userId = id, localDisplayName = name, profile = User.sampleData.profile.copy(displayName = name, fullName = "", image = image), activeUser = active)

private fun chat(info: ChatInfo, items: List<ChatItem> = listOf()) =
  Chat(remoteHostId = null, chatInfo = info, chatItems = items, chatStats = Chat.ChatStats())

private fun call(contact: Contact) =
  Call(remoteHostId = null, userProfile = Profile.sampleData, contact = contact, callUUID = null, callState = CallState.Connected, initialCallType = CallMediaType.Audio, androidCallState = Closeable {})

private val rowPadding = Modifier.padding(start = 8.dp, top = 8.dp, end = 12.dp, bottom = 8.dp)

@Composable
private fun ChatRow(info: ChatInfo) = Box(rowPadding) {
  ChatPreviewView(
    chat(info, listOf(ChatItem.getSampleData(1, CIDirection.DirectRcv(), previewDate, "See you at the call"))),
    true, null, null, null, disabled = false, linkMode = SimplexLinkMode.DESCRIPTION, inProgress = false, progressByTimeout = false, {}
  )
}

@Composable
fun AvatarChatListPeople() = SimpleXTheme {
  Column {
    ChatRow(ChatInfo.Direct(contact(1, "alice", null)))
    ChatRow(ChatInfo.Direct(contact(2, "bob", PHOTO_ORANGE)))
    ChatRow(ChatInfo.Direct(contact(3, "helper", null, ChatPeerType.Bot)))
    ChatRow(ChatInfo.Direct(contact(4, "assistant", null, ChatPeerType.Agent)))
    ChatRow(ChatInfo.Local.sampleData)
    Box(rowPadding) { ContactRequestView(ChatInfo.ContactRequest(UserContactRequest(1, VersionRange(1, 1), "alice", LocalProfile.sampleData, previewDate, previewDate))) }
    Box(rowPadding) { ContactConnectionView(PendingContactConnection(1, "abcd", ConnStatus.New, viaContactUri = false, localAlias = "", createdAt = previewDate, updatedAt = previewDate)) }
  }
}

@Composable
fun AvatarChatListGroups() = SimpleXTheme {
  Column {
    ChatRow(ChatInfo.Group(group(1, "team", null), null))
    ChatRow(ChatInfo.Group(group(2, "friends", PHOTO_PURPLE), null))
    ChatRow(ChatInfo.Group(group(3, "news", null, channel = true), null))
    ChatRow(ChatInfo.Group(group(4, "shop", null, business = true), null))
  }
}

@Composable
fun AvatarToolbars() = SimpleXTheme {
  Column(Modifier.padding(8.dp), verticalArrangement = Arrangement.spacedBy(8.dp)) {
    ChatInfoToolbarTitle(ChatInfo.Direct(contact(1, "alice", null)))
    ChatInfoToolbarTitle(ChatInfo.Direct(contact(2, "bob", PHOTO_ORANGE)))
    ChatInfoToolbarTitle(ChatInfo.Direct(contact(3, "carol", null).copy(activeConn = Connection.sampleData.copy(customUserProfileId = 1))))
    ChatInfoToolbarTitle(ChatInfo.Group(group(1, "team", null), null))
    MemberSupportChatToolbarTitle(member(2, "bob", PHOTO_ORANGE))
    MemberSupportChatToolbarTitle(member(3, "carol", null))
    Row {
      UserProfileButton(null, allRead = false) {}
      UserProfileButton(PHOTO_ORANGE, allRead = false) {}
      ActiveCallInteractiveArea(call(contact(1, "alice", null)))
      ActiveCallInteractiveArea(call(contact(2, "bob", PHOTO_ORANGE)))
    }
  }
}

@Composable
fun AvatarRows() = SimpleXTheme {
  Column {
    UserProfileRow(User.sampleData, enabled = true)
    UserProfileRow(User.sampleData.copy(profile = User.sampleData.profile.copy(image = PHOTO_ORANGE)), enabled = true)
    MemberRow(member(2, "bob", PHOTO_ORANGE))
    MemberRow(member(3, "carol", null))
    SupportChatRow(member(2, "bob", PHOTO_ORANGE), isChannel = false)
    ContactCheckRow(contact(2, "bob", PHOTO_ORANGE), GroupInfo.sampleData, {}, {}, checked = true, enabled = true)
    ContactCheckRow(contact(1, "alice", null), GroupInfo.sampleData, {}, {}, checked = false, enabled = true)
  }
}

@Composable
fun AvatarLinksAndBadge() = SimpleXTheme {
  Column(verticalArrangement = Arrangement.spacedBy(8.dp)) {
    CIChatLinkHeader(MsgChatLink.Contact("https://simplex.chat/contact", Profile.sampleData.copy(image = PHOTO_ORANGE), business = false), null, hasText = false)
    CIChatLinkHeader(MsgChatLink.Group("https://simplex.chat/contact", GroupProfile.sampleData), null, hasText = false)
    ComposeChatLinkView(MsgChatLink.Contact("https://simplex.chat/contact", Profile.sampleData, business = false), cancelEnabled = true) {}
    BadgeUserPreview(BadgeLevel.Supporter)
  }
}

@Composable
fun AvatarLists() = SimpleXTheme {
  Column(Modifier.verticalScroll(rememberScrollState())) {
    ShareListNavLinkView(chat(ChatInfo.Direct(contact(2, "bob", PHOTO_ORANGE))), ChatModel, isMediaOrFileAttachment = false, isVoice = false, hasSimplexLink = false)
    ShareListNavLinkView(chat(ChatInfo.Group(group(1, "team", null), null)), ChatModel, isMediaOrFileAttachment = false, isVoice = false, hasSimplexLink = false)
    ShareListNavLinkView(chat(ChatInfo.Local.sampleData), ChatModel, isMediaOrFileAttachment = false, isVoice = false, hasSimplexLink = false)
    ContactPreviewView(chat(ChatInfo.Direct(contact(1, "alice", null))), disabled = false, showDeletedChatIcon = false)
    ContactPreviewView(chat(ChatInfo.Direct(contact(2, "bob", PHOTO_ORANGE))), disabled = false, showDeletedChatIcon = false)
    ChatListNavLinkView(chat(ChatInfo.InvalidJSON("{}", createdAt = previewDate, updatedAt = previewDate)), remember { mutableStateOf(false) })
    UserPickerUsersSection(
      listOf(UserInfo(user(1, "alice", null, active = true), 0), UserInfo(user(2, "bob", PHOTO_ORANGE, active = false), 3)),
      MaterialTheme.colors.secondaryVariant,
      stopped = false
    ) {}
    Row { ProfilePreview(user(2, "bob", PHOTO_ORANGE, active = false).profile) }
    AppearanceScope.ProfileImageSection()
  }
}

@Composable
fun AvatarChannelMembers() = SimpleXTheme {
  ModalData().ChannelMembersView(null, group(3, "news", null, channel = true), ChatModel, close = {}, showMemberInfo = {})
}
