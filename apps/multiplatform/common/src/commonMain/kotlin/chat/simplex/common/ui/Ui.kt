package chat.simplex.common.ui

import androidx.compose.runtime.Immutable
import androidx.compose.ui.Modifier
import androidx.compose.ui.platform.testTag

@Immutable
data class UiKey(val id: String, val arg: Any? = null) {
  val tag: String get() = if (arg == null) id else "$id[$arg]"
  fun option(value: Any) = UiKey("$id.option", value)
  fun part(name: String) = UiKey("$id.$name", arg)
}

@PublishedApi
@Volatile
internal var uiTestHooksEnabled = false

fun Modifier.ui(key: UiKey?): Modifier = if (key == null || !uiTestHooksEnabled) this else testTag(key.tag)

object Ui {
  object ChatList {
    val NewChat = UiKey("chatList.newChat")
    val NewChatFab = UiKey("chatList.newChatFab")
    val UserPicker = UiKey("chatList.userPicker")
    val Search = UiKey("chatList.search")
    val Bar = UiKey("chatList.bar")
    val Stopped = UiKey("chatList.stopped")
    val Subscriptions = UiKey("chatList.subscriptions")
    val UnreadFilter = UiKey("chatList.unreadFilter")
    val NoUnreadChats = UiKey("chatList.noUnreadChats")
    val ConnectByName = UiKey("chatList.connectByName")
    val OneHandCard = UiKey("chatList.oneHandCard")
    val ConnectBanner = UiKey("chatList.connectBanner")
    val PresetMenu = UiKey("chatList.presetMenu")
    fun PresetTag(kind: String) = UiKey("chatList.presetTag", kind)
    fun PresetMenuItem(kind: String) = UiKey("chatList.presetMenu.item", kind)
    val PresetMenuAll = UiKey("chatList.presetMenu.all")
    val AddList = UiKey("chatList.addList")
    fun UserTag(tagId: Long) = UiKey("chatList.userTag", tagId)
    fun UserTagMenu(tagId: Long, action: String) = UiKey("chatList.userTag.menu.$action", tagId)
    fun Row(chatId: String, remoteHostId: Long? = null) = UiKey("chatList.row", rowArg(chatId, remoteHostId))
    fun RowMenu(chatId: String, action: String, remoteHostId: Long? = null) = UiKey("chatList.row.menu.$action", rowArg(chatId, remoteHostId))
    private fun rowArg(chatId: String, rh: Long?) = if (rh == null) chatId else "$rh:$chatId"
  }
  object TagList {
    fun Tag(tagId: Long) = UiKey("tagList.tag", tagId)
  }
  object TagEditor {
    val Emoji = UiKey("tagEditor.emoji")
    val Name = UiKey("tagEditor.name")
    val Save = UiKey("tagEditor.save")
  }
  object NewChat {
    val CreateLink = UiKey("newChat.createLink")
    val CreateGroup = UiKey("newChat.createGroup")
  }
  object UserPicker {
    fun User(userId: Long) = UiKey("userPicker.user", userId)
    val Address = UiKey("userPicker.address")
    val Settings = UiKey("userPicker.settings")
    val Profiles = UiKey("userPicker.profiles")
    val Sheet = UiKey("userPicker.sheet")
  }
  object Settings {
    val Appearance = UiKey("settings.appearance")
    val Help = UiKey("settings.help")
  }
  object Help {
    val WhatsNew = UiKey("help.whatsNew")
  }
  object WhatsNew {
    val Ok = UiKey("whatsNew.ok")
    val ViewConditions = UiKey("whatsNew.viewConditions")
  }
  object ServersSummary {
    val ShowPercentage = UiKey("serversSummary.showPercentage")
  }
  object Onboarding {
    val GetStarted = UiKey("onboarding.getStarted")
    val ProfileName = UiKey("onboarding.profileName")
    val CreateProfile = UiKey("onboarding.createProfile")
    val LinkAMobile = UiKey("onboarding.linkAMobile")
    val WhyBuilt = UiKey("onboarding.whyBuilt")
    val Migrate = UiKey("onboarding.migrate")
    val NewPassphrase = UiKey("onboarding.newPassphrase")
    val ConfirmPassphrase = UiKey("onboarding.confirmPassphrase")
    val SetPassphrase = UiKey("onboarding.setPassphrase")
    val RandomPassphrase = UiKey("onboarding.randomPassphrase")
    val SetupRouters = UiKey("onboarding.setupRouters")
    val SetupNotifications = UiKey("onboarding.setupNotifications")
    val Continue = UiKey("onboarding.continue")
    fun Operator(operatorId: Long) = UiKey("onboarding.operator", operatorId)
    val OperatorsOk = UiKey("onboarding.operatorsOk")
    val HowItHelps = UiKey("onboarding.howItHelps")
    val ConditionsLink = UiKey("onboarding.conditionsLink")
    val Accept = UiKey("onboarding.accept")
    val ConditionsAccept = UiKey("onboarding.conditionsAccept")
  }
  object Remote {
    val DeviceName = UiKey("remote.deviceName")
    val ShareLink = UiKey("remote.shareLink")
  }
  object Appearance {
    val ColorMode = UiKey("appearance.colorMode")
    val FontScale = UiKey("appearance.fontScale")
    val Zoom = UiKey("appearance.zoom")
  }
  object Address {
    val Create = UiKey("address.create")
    val Share = UiKey("address.share")
  }
  object NewGroup {
    val Name = UiKey("newGroup.name")
    val Create = UiKey("newGroup.create")
  }
  object Profile {
    val Name = UiKey("profile.name")
    val Save = UiKey("profile.save")
  }
  object Profiles {
    val Add = UiKey("profiles.add")
  }
  object NewProfile {
    val Name = UiKey("newProfile.name")
    val Create = UiKey("newProfile.create")
  }
  object Link {
    val Text = UiKey("link.text")
    val ShareProfile = UiKey("link.shareProfile")
    val ProfileIncognito = UiKey("link.profile.incognito")
  }
  object Chat {
    val Bar = UiKey("chat.bar")
    val Message = UiKey("chat.compose")
    val Send = UiKey("chat.send")
    val Connect = UiKey("chat.connect")
    val BarMenu = UiKey("chat.bar.menu")
    fun BarMenuItem(action: String) = UiKey("chat.bar.menu.$action")
    val Selection = UiKey("chat.selection")
    val RequestAccept = UiKey("chat.request.accept")
    val RequestReject = UiKey("chat.request.reject")
    fun Item(itemId: Long) = UiKey("chat.item", itemId)
    fun ItemMenu(itemId: Long, action: String) = UiKey("chat.item.menu.$action", itemId)
    fun ItemReaction(itemId: Long, emoji: String) = UiKey("chat.item.menu.react", "$itemId:$emoji")
    fun ReactionChip(itemId: Long, emoji: String) = UiKey("chat.item.reaction", "$itemId:$emoji")
  }
  object Share {
    fun Row(chatId: String) = UiKey("share.row", chatId)
  }
  object Info {
    val Alias = UiKey("info.alias")
    val DeleteContact = UiKey("info.deleteContact")
    val VerifyCode = UiKey("info.verifyCode")
  }
  object VerifyCode {
    val Code = UiKey("verifyCode.code")
    val Mark = UiKey("verifyCode.mark")
  }
  object InviteMembers {
    fun Contact(contactId: Long) = UiKey("inviteMembers.contact", contactId)
    val Invite = UiKey("inviteMembers.invite")
  }
  object GroupInfo {
    fun Member(groupMemberId: Long) = UiKey("groupInfo.member", groupMemberId)
  }
  object MemberInfo {
    val Role = UiKey("memberInfo.role")
    val Remove = UiKey("memberInfo.remove")
  }
  enum class AlertSource { Shared, PrivacySensitive }
  object Alert {
    fun Confirm(src: AlertSource) = UiKey("alert.confirm", src)
    fun Dismiss(src: AlertSource) = UiKey("alert.dismiss", src)
    fun Option(src: AlertSource, name: String) = UiKey("alert.option.$name", src)
  }
}
