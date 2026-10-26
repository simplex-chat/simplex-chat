package chat.simplex.common.ui

import androidx.compose.runtime.Immutable
import androidx.compose.ui.Modifier
import androidx.compose.ui.platform.testTag
import chat.simplex.common.model.ChatFeature
import chat.simplex.common.model.GroupFeature
import chat.simplex.common.model.GroupSndStatus
import kotlin.reflect.KClass

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

inline fun Modifier.ui(key: () -> UiKey?): Modifier = if (uiTestHooksEnabled) ui(key()) else this

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
    val CreateChannel = UiKey("newChat.createChannel")
    val PasteLink = UiKey("newChat.pasteLink")
    val Search = UiKey("newChat.search")
    fun Contact(chatId: String) = UiKey("newChat.contact", chatId)
    fun ContactMenu(chatId: String, action: String) = UiKey("newChat.contact.menu.$action", chatId)
    val Archived = UiKey("newChat.archived")
    val Favorites = UiKey("newChat.favorites")
  }
  object UserPicker {
    fun User(userId: Long) = UiKey("userPicker.user", userId)
    val Address = UiKey("userPicker.address")
    val ChatPreferences = UiKey("userPicker.chatPreferences")
    val LinkMobile = UiKey("userPicker.linkMobile")
    val Settings = UiKey("userPicker.settings")
    val Profiles = UiKey("userPicker.profiles")
    val Sheet = UiKey("userPicker.sheet")
    val ColorMode = UiKey("userPicker.colorMode")
    val UseFromDesktop = UiKey("userPicker.useFromDesktop")
    fun Device(remoteHostId: Long?) = UiKey("userPicker.device", remoteHostId ?: "local")
  }
  object Settings {
    val Appearance = UiKey("settings.appearance")
    val Help = UiKey("settings.help")
    val Privacy = UiKey("settings.privacy")
    val Database = UiKey("settings.database")
    val Network = UiKey("settings.network")
    val Calls = UiKey("settings.calls")
    val Version = UiKey("settings.version")
    val DeveloperTools = UiKey("settings.developerTools")
    val Migrate = UiKey("settings.migrate")
    val Notifications = UiKey("settings.notifications")
  }
  object Notifications {
    val Mode = UiKey("notifications.mode")
  }
  object Network {
    fun Operator(operatorId: Long) = UiKey("network.operator", operatorId)
    val YourServers = UiKey("network.yourServers")
    val UseSocks = UiKey("network.useSocks")
    val SocksSettings = UiKey("network.socksSettings")
    val Advanced = UiKey("network.advanced")
    val SaveServers = UiKey("network.saveServers")
  }
  object Servers {
    fun Server(server: String) = UiKey("servers.server", server)
    val Add = UiKey("servers.add")
    val TestAll = UiKey("servers.testAll")
    val Address = UiKey("servers.address")
    val Test = UiKey("servers.test")
    val Use = UiKey("servers.use")
    val Delete = UiKey("servers.delete")
  }
  object Operator {
    val Info = UiKey("operator.info")
    val Use = UiKey("operator.use")
    val AcceptConditions = UiKey("operator.acceptConditions")
  }
  object AdvancedNetwork {
    val ProxyMode = UiKey("advancedNetwork.proxyMode")
    val ProxyFallback = UiKey("advancedNetwork.proxyFallback")
    fun Setting(name: String) = UiKey("advancedNetwork.setting.$name")
    val KeepAlive = UiKey("advancedNetwork.keepAlive")
    val SessionMode = UiKey("advancedNetwork.sessionMode")
    val Reset = UiKey("advancedNetwork.reset")
    val Save = UiKey("advancedNetwork.save")
  }
  object Socks {
    val Host = UiKey("socks.host")
    val Port = UiKey("socks.port")
    val OnionHosts = UiKey("socks.onionHosts")
    val Save = UiKey("socks.save")
    val Reset = UiKey("socks.reset")
  }
  object Privacy {
    val More = UiKey("privacy.more")
    val ShowLastMessages = UiKey("privacy.showLastMessages")
    val Lock = UiKey("privacy.lock")
    val LinkPreviews = UiKey("privacy.linkPreviews")
    val AcceptImages = UiKey("privacy.acceptImages")
    val BlurMedia = UiKey("privacy.blurMedia")
    val ReceiptsContacts = UiKey("privacy.receiptsContacts")
    val ReceiptsGroups = UiKey("privacy.receiptsGroups")
    val SanitizeLinks = UiKey("privacy.sanitizeLinks")
    val AutoAcceptMemberContacts = UiKey("privacy.autoAcceptMemberContacts")
    val AutoAcceptGroupInvitations = UiKey("privacy.autoAcceptGroupInvitations")
    val SaveDraft = UiKey("privacy.saveDraft")
    val EncryptLocalFiles = UiKey("privacy.encryptLocalFiles")
  }
  object DeliveryReceipts {
    val Enable = UiKey("deliveryReceipts.enable")
    val Skip = UiKey("deliveryReceipts.skip")
  }
  object Lock {
    val Enable = UiKey("lock.enable")
    val Delay = UiKey("lock.delay")
    val ChangePasscode = UiKey("lock.changePasscode")
    val SelfDestruct = UiKey("lock.selfDestruct")
    val SelfDestructName = UiKey("lock.selfDestructName")
    val SelfDestructInfo = UiKey("lock.selfDestructInfo")
    val ChangeSelfDestruct = UiKey("lock.changeSelfDestruct")
    val Unlock = UiKey("lock.unlock")
  }
  object Passcode {
    fun Digit(d: Int) = UiKey("passcode.digit", d)
    val Backspace = UiKey("passcode.backspace")
    val Clear = UiKey("passcode.clear")
    val Submit = UiKey("passcode.submit")
    val Cancel = UiKey("passcode.cancel")
    val Entered = UiKey("passcode.entered")
  }
  object Help {
    val WhatsNew = UiKey("help.whatsNew")
    val HowToUse = UiKey("help.howToUse")
    val About = UiKey("help.about")
    val News = UiKey("help.news")
    val Founder = UiKey("help.founder")
    val Email = UiKey("help.email")
    val Contribute = UiKey("help.contribute")
    val Star = UiKey("help.star")
  }
  object DevTools {
    val Console = UiKey("devTools.console")
    val ResetHints = UiKey("devTools.resetHints")
    val ShowOptions = UiKey("devTools.showOptions")
  }
  object WhatsNew {
    val Ok = UiKey("whatsNew.ok")
    val ViewConditions = UiKey("whatsNew.viewConditions")
  }
  object ServersSummary {
    val ShowPercentage = UiKey("serversSummary.showPercentage")
    val ResetStats = UiKey("serversSummary.resetStats")
    val ReconnectAll = UiKey("serversSummary.reconnectAll")
    val Details = UiKey("serversSummary.details")
    fun SmpServer(host: String) = UiKey("serversSummary.smpServer", host)
    val UserCategory = UiKey("serversSummary.userCategory")
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
    fun NotificationsModeCard(mode: String) = UiKey("onboarding.notificationsModeCard", mode)
    val NotificationsOk = UiKey("onboarding.notificationsOk")
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
    val AddMobile = UiKey("remote.addMobile")
    val PasteAddress = UiKey("remote.pasteAddress")
    val Connect = UiKey("remote.connect")
    val Confirm = UiKey("remote.confirm")
    val Disconnect = UiKey("remote.disconnect")
  }
  object Appearance {
    val ColorMode = UiKey("appearance.colorMode")
    val FontScale = UiKey("appearance.fontScale")
    val Zoom = UiKey("appearance.zoom")
    val Language = UiKey("appearance.language")
    val OneHandUI = UiKey("appearance.oneHandUI")
    val DarkModeColors = UiKey("appearance.darkModeColors")
    val ApplyTo = UiKey("appearance.applyTo")
    fun Wallpaper(preset: String) = UiKey("appearance.wallpaper", preset)
    val WallpaperImage = UiKey("appearance.wallpaperImage")
    val RemoveImage = UiKey("appearance.removeImage")
    val Customize = UiKey("appearance.customize")
    val Accent = UiKey("appearance.accent")
    val ColorHex = UiKey("appearance.colorHex")
    val ResetColors = UiKey("appearance.resetColors")
    val ExportTheme = UiKey("appearance.exportTheme")
    val ImportTheme = UiKey("appearance.importTheme")
    val ChatBottomBar = UiKey("appearance.chatBottomBar")
  }
  object Address {
    val Create = UiKey("address.create")
    val Share = UiKey("address.share")
    val Delete = UiKey("address.delete")
    val Business = UiKey("address.business")
    val Settings = UiKey("address.settings")
    val AutoAccept = UiKey("address.autoAccept")
    val WelcomeMessage = UiKey("address.welcomeMessage")
    val SaveSettings = UiKey("address.saveSettings")
    val ShareWithContacts = UiKey("address.shareWithContacts")
    val ShareViaChat = UiKey("address.shareViaChat")
    val CreateLink = UiKey("address.createLink")
    val LearnMore = UiKey("address.learnMore")
  }
  object NewGroup {
    val Name = UiKey("newGroup.name")
    val Create = UiKey("newGroup.create")
    val Incognito = UiKey("newGroup.incognito")
  }
  object NewChannel {
    val Name = UiKey("newChannel.name")
    val Create = UiKey("newChannel.create")
  }
  object Profile {
    val Name = UiKey("profile.name")
    val FullName = UiKey("profile.fullName")
    val Bio = UiKey("profile.bio")
    val Description = UiKey("profile.description")
    val DescriptionText = UiKey("profile.descriptionText")
    val Save = UiKey("profile.save")
  }
  object Profiles {
    val Add = UiKey("profiles.add")
    val Bar = UiKey("profiles.bar")
    fun User(userId: Long) = UiKey("profiles.user", userId)
    fun UserMenu(userId: Long, action: String) = UiKey("profiles.user.menu.$action", userId)
  }
  object HideProfile {
    val Password = UiKey("hideProfile.password")
    val Confirm = UiKey("hideProfile.confirm")
    val Save = UiKey("hideProfile.save")
  }
  object ProfileAction {
    val Password = UiKey("profileAction.password")
    val Submit = UiKey("profileAction.submit")
  }
  object NewProfile {
    val Name = UiKey("newProfile.name")
    val Create = UiKey("newProfile.create")
  }
  object Link {
    val Text = UiKey("link.text")
    val ShareProfile = UiKey("link.shareProfile")
    val ProfileIncognito = UiKey("link.profile.incognito")
    fun Profile(userId: Long) = UiKey("link.profile", userId)
    val ProfileBar = UiKey("link.profileBar")
    val Paste = UiKey("link.paste")
    val ShortLinkToggle = UiKey("link.shortLinkToggle")
  }
  object Chat {
    val Bar = UiKey("chat.bar")
    val Message = UiKey("chat.compose")
    val Send = Message.part("send")
    val SendLive = Message.part("sendLive")
    val Live = Message.part("live")
    val ClearText = Message.part("clearText")
    val SendTimed = Message.part("timed")
    val ScopeMessage = UiKey("chat.compose", "scope")
    val ScopeSend = ScopeMessage.part("send")
    val Connect = UiKey("chat.connect")
    val BarMenu = UiKey("chat.bar.menu")
    fun BarMenuItem(action: String) = UiKey("chat.bar.menu.$action")
    val Selection = UiKey("chat.selection")
    val TextSelectionCopy = UiKey("chat.textSelection.copy")
    val RequestAccept = UiKey("chat.request.accept")
    val RequestReject = UiKey("chat.request.reject")
    val PendingAccept = UiKey("chat.pending.accept")
    val PendingReject = UiKey("chat.pending.reject")
    fun Item(itemId: Long) = UiKey("chat.item", itemId)
    fun ItemMenu(itemId: Long, action: String) = UiKey("chat.item.menu.$action", itemId)
    fun ItemFile(itemId: Long) = UiKey("chat.item.file", itemId)
    val ScrollToBottom = UiKey("chat.scrollToBottom")
    val UnreadAbove = UiKey("chat.unreadAbove")
    val SearchButton = UiKey("chat.searchButton")
    val ContentFilter = UiKey("chat.contentFilter")
    fun ItemReaction(itemId: Long, emoji: String) = UiKey("chat.item.menu.react", "$itemId:$emoji")
    fun ReactionChip(itemId: Long, emoji: String) = UiKey("chat.item.reaction", "$itemId:$emoji")
    fun ReactionMember(itemId: Long, name: String) = UiKey("chat.item.reaction.member", "$itemId:$name")
    val FullScreenMedia = UiKey("chat.fullScreenMedia")
    fun MentionMember(groupMemberId: Long) = UiKey("chat.mentionMember", groupMemberId)
    fun PickerUser(userId: Long) = UiKey("chat.picker.user", userId)
    val PickerIncognito = UiKey("chat.picker.incognito")
    val Reports = UiKey("chat.reports")
    val SupportChats = UiKey("chat.supportChats")
    fun ReportReason(label: String) = UiKey("chat.reportReason", label)
    fun Attach(option: String) = UiKey("chat.attach", option)
    val CommandsButton = UiKey("chat.commands")
    fun Command(keyword: String) = UiKey("chat.commands.command", keyword)
    fun CommandMenu(label: String) = UiKey("chat.commands.menu", label)
    val CommandsBack = UiKey("chat.commands.back")
  }
  object TimePicker {
    val Value = UiKey("timePicker.value")
    val Confirm = UiKey("timePicker.confirm")
  }
  object ItemInfo {
    fun Member(groupMemberId: Long) = UiKey("itemInfo.member", groupMemberId)
    fun MemberStatus(groupMemberId: Long, status: KClass<out GroupSndStatus>) = UiKey("itemInfo.member.status", "$groupMemberId:${status.simpleName}")
    val Quote = UiKey("itemInfo.quote")
    val ForwardedFrom = UiKey("itemInfo.forwardedFrom")
    val Share = UiKey("itemInfo.share")
  }
  object Share {
    fun Row(chatId: String) = UiKey("share.row", chatId)
    val Bar = UiKey("share.bar")
  }
  object DbError {
    val Passphrase = UiKey("dbError.passphrase")
    val Open = UiKey("dbError.open")
  }

  object Info {
    val Alias = UiKey("info.alias")
    val DeleteContact = UiKey("info.deleteContact")
    val ClearChat = UiKey("info.clearChat")
    val VerifyCode = UiKey("info.verifyCode")
    val Preferences = UiKey("info.preferences")
    val Name = UiKey("info.name")
    val Header = UiKey("info.header")
    val Mute = UiKey("info.mute")
    val SwitchAddress = UiKey("info.switchAddress")
    val AbortSwitchAddress = UiKey("info.abortSwitchAddress")
    val ReceivingVia = UiKey("info.receivingVia")
    val SendingVia = UiKey("info.sendingVia")
    val Call = UiKey("info.call")
    val SendReceipts = UiKey("info.sendReceipts")
    val Ttl = UiKey("info.ttl")
  }
  object ContactPrefs {
    fun Allow(feature: ChatFeature) = UiKey("contactPrefs.allow", feature.name)
    val Timed = UiKey("contactPrefs.timed")
    val Ttl = UiKey("contactPrefs.ttl")
    val Reset = UiKey("contactPrefs.reset")
    val Save = UiKey("contactPrefs.save")
  }
  object UserPrefs {
    fun Allow(feature: ChatFeature) = UiKey("userPrefs.allow", feature.name)
    val Timed = UiKey("userPrefs.timed")
    val Reset = UiKey("userPrefs.reset")
    val Save = UiKey("userPrefs.save")
  }
  object VerifyCode {
    val Code = UiKey("verifyCode.code")
    val Mark = UiKey("verifyCode.mark")
  }
  object InviteMembers {
    fun Contact(contactId: Long) = UiKey("inviteMembers.contact", contactId)
    val Invite = UiKey("inviteMembers.invite")
    val Role = UiKey("inviteMembers.role")
    val Skip = UiKey("inviteMembers.skip")
    val Search = UiKey("inviteMembers.search")
    val Clear = UiKey("inviteMembers.clear")
  }
  object GroupInfo {
    fun Member(groupMemberId: Long) = UiKey("groupInfo.member", groupMemberId)
    val CreateLink = UiKey("groupInfo.createLink")
    val Preferences = UiKey("groupInfo.preferences")
    val MemberSupport = UiKey("groupInfo.memberSupport")
    val UserSupport = UiKey("groupInfo.userSupport")
    val AddMembers = UiKey("groupInfo.addMembers")
    val EditProfile = UiKey("groupInfo.editProfile")
    val WelcomeMessage = UiKey("groupInfo.welcomeMessage")
    val Leave = UiKey("groupInfo.leave")
    val Delete = UiKey("groupInfo.delete")
    val Reports = UiKey("groupInfo.reports")
    val Content = UiKey("groupInfo.content")
    val Mute = UiKey("groupInfo.mute")
    val SendReceipts = UiKey("groupInfo.sendReceipts")
    val GroupLink = UiKey("groupInfo.groupLink")
    val MembersToolbar = UiKey("groupInfo.membersToolbar")
    fun MemberMenu(groupMemberId: Long, action: String) = UiKey("groupInfo.member.menu.$action", groupMemberId)
  }
  object GroupProfile {
    val Name = UiKey("groupProfile.name")
    val ShortDescr = UiKey("groupProfile.shortDescr")
    val Save = UiKey("groupProfile.save")
  }
  object Welcome {
    val Text = UiKey("welcome.text")
    val Preview = UiKey("welcome.preview")
    val Save = UiKey("welcome.save")
  }
  object GroupLink {
    val Share = UiKey("groupLink.share")
    val Continue = UiKey("groupLink.continue")
    val Role = UiKey("groupLink.role")
  }
  object MemberInfo {
    val Role = UiKey("memberInfo.role")
    val Remove = UiKey("memberInfo.remove")
    val BlockForAll = UiKey("memberInfo.blockForAll")
    val UnblockForAll = UiKey("memberInfo.unblockForAll")
    val Block = UiKey("memberInfo.block")
    val Unblock = UiKey("memberInfo.unblock")
    val Message = UiKey("memberInfo.message")
    val Call = UiKey("memberInfo.call")
    val Video = UiKey("memberInfo.video")
    val DeleteMessages = UiKey("memberInfo.deleteMessages")
    val SupportChat = UiKey("memberInfo.supportChat")
  }
  enum class AlertSource { Shared, PrivacySensitive }
  object Alert {
    fun Confirm(src: AlertSource) = UiKey("alert.confirm", src)
    fun Dismiss(src: AlertSource) = UiKey("alert.dismiss", src)
    fun Option(src: AlertSource, name: String) = UiKey("alert.option.$name", src)
    fun ProfileName(src: AlertSource) = UiKey("alert.profileName", src)
  }
  object GroupPrefs {
    val MemberAdmission = UiKey("groupPrefs.memberAdmission")
    fun Toggle(feature: GroupFeature) = UiKey("groupPrefs.toggle", feature.name)
    fun Role(feature: GroupFeature) = UiKey("groupPrefs.role", feature.name)
    val Ttl = UiKey("groupPrefs.ttl")
    val Reset = UiKey("groupPrefs.reset")
    val Save = UiKey("groupPrefs.save")
  }
  object MemberAdmission {
    val Review = UiKey("memberAdmission.review")
    val Save = UiKey("memberAdmission.save")
  }
  object MemberSupport {
    val Search = UiKey("memberSupport.search")
    fun Row(groupMemberId: Long) = UiKey("memberSupport.row", groupMemberId)
    fun RowMenu(groupMemberId: Long, action: String) = UiKey("memberSupport.row.menu.$action", groupMemberId)
  }
  object Database {
    val Ttl = UiKey("database.ttl")
    val Management = UiKey("database.management")
    val DeleteFiles = UiKey("database.deleteFiles")
    val Passphrase = UiKey("database.passphrase")
    val Export = UiKey("database.export")
    val Import = UiKey("database.import")
    val Delete = UiKey("database.delete")
    val RunChat = UiKey("database.runChat")
  }
  object DbPassphrase {
    val Save = UiKey("dbPassphrase.save")
    val Current = UiKey("dbPassphrase.current")
    val New = UiKey("dbPassphrase.new")
    val Confirm = UiKey("dbPassphrase.confirm")
    val Update = UiKey("dbPassphrase.update")
  }
}
