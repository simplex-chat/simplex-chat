package chat.simplex.common.model

import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.runtime.snapshots.SnapshotStateList
import androidx.compose.runtime.snapshots.SnapshotStateMap
import androidx.compose.ui.graphics.*
import androidx.compose.ui.text.SpanStyle
import androidx.compose.ui.text.font.*
import androidx.compose.ui.text.style.TextDecoration
import chat.simplex.common.model.ChatModel.chatItemsChangesListener
import chat.simplex.common.platform.*
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.call.*
import chat.simplex.common.views.chat.*
import chat.simplex.common.views.helpers.*
import chat.simplex.common.views.migration.MigrationToDeviceState
import chat.simplex.common.views.migration.MigrationToState
import chat.simplex.res.MR
import dev.icerock.moko.resources.ImageResource
import dev.icerock.moko.resources.StringResource
import kotlinx.coroutines.*
import kotlinx.coroutines.flow.*
import kotlinx.coroutines.sync.Mutex
import kotlinx.coroutines.sync.withLock
import kotlin.collections.removeAll as remAll
import kotlinx.datetime.*
import kotlinx.datetime.TimeZone
import kotlinx.serialization.*
import kotlinx.serialization.descriptors.*
import kotlinx.serialization.encoding.Decoder
import kotlinx.serialization.encoding.Encoder
import kotlinx.serialization.json.*
import java.io.Closeable
import java.io.File
import java.net.URI
import java.time.format.DateTimeFormatter
import java.time.format.FormatStyle
import java.util.*
import kotlin.collections.ArrayList
import kotlin.random.Random
import kotlin.time.*

/*
 * Without this annotation an animation from ChatList to ChatView has 1 frame per the whole animation. Don't delete it
 * */
@Stable
object ChatModel {
  val controller: ChatController = ChatController
  val setDeliveryReceipts = mutableStateOf(false)
  val currentUser = mutableStateOf<User?>(null)
  val users = mutableStateListOf<UserInfo>()
  val localUserCreated = mutableStateOf<Boolean?>(null)
  val chatRunning = mutableStateOf<Boolean?>(null)
  val chatDbChanged = mutableStateOf<Boolean>(false)
  val chatDbEncrypted = mutableStateOf<Boolean?>(false)
  val chatDbStatus = mutableStateOf<DBMigrationResult?>(null)
  val ctrlInitInProgress = mutableStateOf(false)
  val dbMigrationInProgress = mutableStateOf(false)
  val incompleteInitializedDbRemoved = mutableStateOf(false)
  private val _chats = mutableStateOf(SnapshotStateList<Chat>())
  val chats: State<List<Chat>> = _chats
  private val chatsContext = ChatsContext()
  // map of connections network statuses, key is agent connection id
  val networkStatuses = mutableStateMapOf<String, NetworkStatus>()
  val switchingUsersAndHosts = mutableStateOf(false)

  // current chat
  val chatId = mutableStateOf<String?>(null)
  /** if you modify the items by adding/removing them, use helpers methods like [addAndNotify], [removeLastAndNotify], [removeAllAndNotify], [clearAndNotify] and so on.
   * If some helper is missing, create it. Notify is needed to track state of items that we added manually (not via api call). See [apiLoadMessages].
   * If you use api call to get the items, use just [add] instead of [addAndNotify].
   * Never modify underlying list directly because it produces unexpected results in ChatView's LazyColumn (setting by index is ok) */
  val chatItems = mutableStateOf(SnapshotStateList<ChatItem>())
  // set listener here that will be notified on every add/delete of a chat item
  var chatItemsChangesListener: ChatItemsChangesListener? = null
  val chatState = ActiveChatState()
  // rhId, chatId
  val deletedChats = mutableStateOf<List<Pair<Long?, String>>>(emptyList())
  val chatItemStatuses = mutableMapOf<Long, CIStatus>()
  val groupMembers = mutableStateListOf<GroupMember>()
  val groupMembersIndexes = mutableStateMapOf<Long, Int>()

  // false: default placement, true: floating window.
  // Used for deciding to add terminal items on main thread or not. Floating means appPrefs.terminalAlwaysVisible
  var terminalsVisible = setOf<Boolean>()
  val terminalItems = mutableStateOf<List<TerminalItem>>(listOf())
  val userAddress = mutableStateOf<UserContactLinkRec?>(null)
  val chatItemTTL = mutableStateOf<ChatItemTTL>(ChatItemTTL.None)

  // set when app opened from external intent
  val clearOverlays = mutableStateOf<Boolean>(false)

  // Only needed during onboarding when user skipped password setup (left as random password)
  val desktopOnboardingRandomPassword = mutableStateOf(false)

  // set when app is opened via contact or invitation URI (rhId, uri)
  val appOpenUrl = mutableStateOf<Pair<Long?, String>?>(null)

  // Needed to check for bottom nav bar and to apply or not navigation bar color on Android
  val newChatSheetVisible = mutableStateOf(false)

  // Needed to apply black color to left/right cutout area on Android
  val fullscreenGalleryVisible = mutableStateOf(false)

  // preferences
  val notificationPreviewMode by lazy {
    mutableStateOf(
      try {
        NotificationPreviewMode.valueOf(controller.appPrefs.notificationPreviewMode.get()!!)
      } catch (e: Exception) {
        NotificationPreviewMode.default
      }
    )
  }
  val showAuthScreen by lazy { mutableStateOf(ChatController.appPrefs.performLA.get()) }
  val showAdvertiseLAUnavailableAlert = mutableStateOf(false)
  val showChatPreviews by lazy { mutableStateOf(ChatController.appPrefs.privacyShowChatPreviews.get()) }

  // current WebRTC call
  val callManager = CallManager(this)
  val callInvitations = mutableStateMapOf<String, RcvCallInvitation>()
  val activeCallInvitation = mutableStateOf<RcvCallInvitation?>(null)
  val activeCall = mutableStateOf<Call?>(null)
  val activeCallViewIsVisible = mutableStateOf<Boolean>(false)
  val activeCallViewIsCollapsed = mutableStateOf<Boolean>(false)
  val callCommand = mutableStateListOf<WCallCommand>()
  val showCallView = mutableStateOf(false)
  val switchingCall = mutableStateOf(false)

  // currently showing invitation
  val showingInvitation = mutableStateOf(null as ShowingInvitation?)

  val migrationState: MutableState<MigrationToState?> by lazy { mutableStateOf(MigrationToDeviceState.makeMigrationState()) }

  var draft = mutableStateOf(null as ComposeState?)
  var draftChatId = mutableStateOf(null as String?)

  // working with external intents or internal forwarding of chat items
  val sharedContent = mutableStateOf(null as SharedContent?)

  val filesToDelete = mutableSetOf<File>()
  val simplexLinkMode by lazy { mutableStateOf(ChatController.appPrefs.simplexLinkMode.get()) }

  val clipboardHasText = mutableStateOf(false)
  val networkInfo = mutableStateOf(UserNetworkInfo(networkType = UserNetworkType.OTHER, online = true))

  val conditions = mutableStateOf(ServerOperatorConditionsDetail.empty)

  val updatingProgress = mutableStateOf(null as Float?)
  var updatingRequest: Closeable? = null

  private val updatingChatsMutex: Mutex = Mutex()
  val changingActiveUserMutex: Mutex = Mutex()

  val desktopNoUserNoRemote: Boolean @Composable get() = appPlatform.isDesktop && currentUser.value == null && currentRemoteHost.value == null
  fun desktopNoUserNoRemote(): Boolean = appPlatform.isDesktop && currentUser.value == null && currentRemoteHost.value == null

  // remote controller
  val remoteHosts = mutableStateListOf<RemoteHostInfo>()
  val currentRemoteHost = mutableStateOf<RemoteHostInfo?>(null)
  val remoteHostId: Long? @Composable get() = remember { currentRemoteHost }.value?.remoteHostId
  fun remoteHostId(): Long? = currentRemoteHost.value?.remoteHostId
  val remoteHostPairing = mutableStateOf<Pair<RemoteHostInfo?, RemoteHostSessionState>?>(null)
  val remoteCtrlSession = mutableStateOf<RemoteCtrlSession?>(null)

  val processedCriticalError: ProcessedErrors<AgentErrorType.CRITICAL> = ProcessedErrors(60_000)
  val processedInternalError: ProcessedErrors<AgentErrorType.INTERNAL> = ProcessedErrors(20_000)

  // return true if you handled the click
  var centerPanelBackgroundClickHandler: (() -> Boolean)? = null

  fun getUser(userId: Long): User? = if (currentUser.value?.userId == userId) {
    currentUser.value
  } else {
    users.firstOrNull { it.user.userId == userId }?.user
  }

  private fun getUserIndex(user: User): Int =
    users.indexOfFirst { it.user.userId == user.userId && it.user.remoteHostId == user.remoteHostId }

  fun updateUser(user: User) {
    val i = getUserIndex(user)
    if (i != -1) {
      users[i] = users[i].copy(user = user)
    }
    if (currentUser.value?.userId == user.userId) {
      currentUser.value = user
    }
  }

  fun removeUser(user: User) {
    val i = getUserIndex(user)
    if (i != -1) {
      users.removeAt(i)
    }
  }

  // toList() here is to prevent ConcurrentModificationException that is rarely happens but happens
  fun hasChat(rhId: Long?, id: String): Boolean = chats.value.firstOrNull { it.id == id && it.remoteHostId == rhId } != null
  // TODO pass rhId?
  fun getChat(id: String): Chat? = chats.value.firstOrNull { it.id == id }
  fun getContactChat(contactId: Long): Chat? = chats.value.firstOrNull { it.chatInfo is ChatInfo.Direct && it.chatInfo.apiId == contactId }
  fun getGroupChat(groupId: Long): Chat? = chats.value.firstOrNull { it.chatInfo is ChatInfo.Group && it.chatInfo.apiId == groupId }

  fun populateGroupMembersIndexes() {
    groupMembersIndexes.clear()
    groupMembers.forEachIndexed { i, member ->
      groupMembersIndexes[member.groupMemberId] = i
    }
  }

  fun getGroupMember(groupMemberId: Long): GroupMember? {
    val memberIndex = groupMembersIndexes[groupMemberId]
    return if (memberIndex != null) {
      groupMembers[memberIndex]
    } else {
      null
    }
  }

  suspend fun <T> withChats(action: suspend ChatsContext.() -> T): T = updatingChatsMutex.withLock {
    chatsContext.action()
  }

  class ChatsContext {
    val chats = _chats

    suspend fun addChat(chat: Chat) {
      chats.add(index = 0, chat)
      popChatCollector.throttlePopChat(chat.remoteHostId, chat.id, currentPosition = 0)
    }

    private suspend fun reorderChat(chat: Chat, toIndex: Int) {
      val newChats = SnapshotStateList<Chat>()
      newChats.addAll(chats.value)
      newChats.remove(chat)
      newChats.add(index = toIndex, chat)
      chats.replaceAll(newChats)
      popChatCollector.throttlePopChat(chat.remoteHostId, chat.id, currentPosition = toIndex)
    }

    fun updateChatInfo(rhId: Long?, cInfo: ChatInfo) {
      val i = getChatIndex(rhId, cInfo.id)
      if (i >= 0) {
        val currentCInfo = chats[i].chatInfo
        var newCInfo = cInfo
        if (currentCInfo is ChatInfo.Direct && newCInfo is ChatInfo.Direct) {
          val currentStats = currentCInfo.contact.activeConn?.connectionStats
          val newConn = newCInfo.contact.activeConn
          val newStats = newConn?.connectionStats
          if (currentStats != null && newConn != null && newStats == null) {
            newCInfo = newCInfo.copy(
              contact = newCInfo.contact.copy(
                activeConn = newConn.copy(
                  connectionStats = currentStats
                )
              )
            )
          }
        }
        chats[i] = chats[i].copy(chatInfo = newCInfo)
      }
    }

    suspend fun updateContactConnection(rhId: Long?, contactConnection: PendingContactConnection) = updateChat(rhId, ChatInfo.ContactConnection(contactConnection))

    suspend fun updateContact(rhId: Long?, contact: Contact) = updateChat(rhId, ChatInfo.Direct(contact), addMissing = contact.directOrUsed)

    suspend fun updateContactConnectionStats(rhId: Long?, contact: Contact, connectionStats: ConnectionStats) {
      val updatedConn = contact.activeConn?.copy(connectionStats = connectionStats)
      val updatedContact = contact.copy(activeConn = updatedConn)
      updateContact(rhId, updatedContact)
    }

    suspend fun updateGroup(rhId: Long?, groupInfo: GroupInfo) = updateChat(rhId, ChatInfo.Group(groupInfo))

    private suspend fun updateChat(rhId: Long?, cInfo: ChatInfo, addMissing: Boolean = true) {
      if (hasChat(rhId, cInfo.id)) {
        updateChatInfo(rhId, cInfo)
      } else if (addMissing) {
        addChat(Chat(remoteHostId = rhId, chatInfo = cInfo, chatItems = arrayListOf()))
      }
    }

    fun updateChats(newChats: List<Chat>) {
      chats.replaceAll(newChats)
      popChatCollector.clear()

      val cId = chatId.value
      // If chat is null, it was deleted in background after apiGetChats call
      if (cId != null && getChat(cId) == null) {
        chatId.value = null
      }
    }

    suspend fun replaceChat(rhId: Long?, id: String, chat: Chat) {
      val i = getChatIndex(rhId, id)
      if (i >= 0) {
        chats[i] = chat
      } else {
        // invalid state, correcting
        addChat(chat)
      }
    }
    suspend fun addChatItem(rhId: Long?, cInfo: ChatInfo, cItem: ChatItem) {
      // mark chat non deleted
      if (cInfo is ChatInfo.Direct && cInfo.chatDeleted) {
        val updatedContact = cInfo.contact.copy(chatDeleted = false)
        updateContact(rhId, updatedContact)
      }
      // update previews
      val i = getChatIndex(rhId, cInfo.id)
      val chat: Chat
      if (i >= 0) {
        chat = chats[i]
        val newPreviewItem = when (cInfo) {
          is ChatInfo.Group -> {
            val currentPreviewItem = chat.chatItems.firstOrNull()
            if (currentPreviewItem != null) {
              if (cItem.meta.itemTs >= currentPreviewItem.meta.itemTs) {
                cItem
              } else {
                currentPreviewItem
              }
            } else {
              cItem
            }
          }
          else -> cItem
        }
        chats[i] = chat.copy(
          chatItems = arrayListOf(newPreviewItem),
          chatStats =
          if (cItem.meta.itemStatus is CIStatus.RcvNew) {
            increaseUnreadCounter(rhId, currentUser.value!!)
            chat.chatStats.copy(unreadCount = chat.chatStats.unreadCount + 1)
          }
          else
            chat.chatStats
        )
        if (appPlatform.isDesktop && cItem.chatDir.sent) {
          reorderChat(chats[i], 0)
        } else {
          popChatCollector.throttlePopChat(chat.remoteHostId, chat.id, currentPosition = i)
        }
      } else {
        addChat(Chat(remoteHostId = rhId, chatInfo = cInfo, chatItems = arrayListOf(cItem)))
      }
      withContext(Dispatchers.Main) {
        // add to current chat
        if (chatId.value == cInfo.id) {
          // Prevent situation when chat item already in the list received from backend
          if (chatItems.value.none { it.id == cItem.id }) {
            if (chatItems.value.lastOrNull()?.id == ChatItem.TEMP_LIVE_CHAT_ITEM_ID) {
              chatItems.addAndNotify(kotlin.math.max(0, chatItems.value.lastIndex), cItem)
            } else {
              chatItems.addAndNotify(cItem)
            }
          }
        }
      }
    }

    suspend fun upsertChatItem(rhId: Long?, cInfo: ChatInfo, cItem: ChatItem): Boolean {
      // update previews
      val i = getChatIndex(rhId, cInfo.id)
      val chat: Chat
      val res: Boolean
      if (i >= 0) {
        chat = chats[i]
        val pItem = chat.chatItems.lastOrNull()
        if (pItem?.id == cItem.id) {
          chats[i] = chat.copy(chatItems = arrayListOf(cItem))
          if (pItem.isRcvNew && !cItem.isRcvNew) {
            // status changed from New to Read, update counter
            decreaseCounterInChat(rhId, cInfo.id)
          }
        }
        res = false
      } else {
        addChat(Chat(remoteHostId = rhId, chatInfo = cInfo, chatItems = arrayListOf(cItem)))
        res = true
      }
      return withContext(Dispatchers.Main) {
        // update current chat
        if (chatId.value == cInfo.id) {
          if (cItem.isDeletedContent || cItem.meta.itemDeleted != null) {
            AudioPlayer.stop(cItem)
          }
          val items = chatItems.value
          val itemIndex = items.indexOfFirst { it.id == cItem.id }
          if (itemIndex >= 0) {
            items[itemIndex] = cItem
            false
          } else {
            val status = chatItemStatuses.remove(cItem.id)
            val ci = if (status != null && cItem.meta.itemStatus is CIStatus.SndNew) {
              cItem.copy(meta = cItem.meta.copy(itemStatus = status))
            } else {
              cItem
            }
            chatItems.addAndNotify(ci)
            true
          }
        } else {
          res
        }
      }
    }

    suspend fun updateChatItem(cInfo: ChatInfo, cItem: ChatItem, status: CIStatus? = null) {
      withContext(Dispatchers.Main) {
        if (chatId.value == cInfo.id) {
          val items = chatItems.value
          val itemIndex = items.indexOfFirst { it.id == cItem.id }
          if (itemIndex >= 0) {
            items[itemIndex] = cItem
          }
        } else if (status != null) {
          chatItemStatuses[cItem.id] = status
        }
      }
    }

    fun removeChatItem(rhId: Long?, cInfo: ChatInfo, cItem: ChatItem) {
      if (cItem.isRcvNew) {
        decreaseCounterInChat(rhId, cInfo.id)
      }
      // update previews
      val i = getChatIndex(rhId, cInfo.id)
      val chat: Chat
      if (i >= 0) {
        chat = chats[i]
        val pItem = chat.chatItems.lastOrNull()
        if (pItem?.id == cItem.id) {
          chats[i] = chat.copy(chatItems = arrayListOf(ChatItem.deletedItemDummy))
        }
      }
      // remove from current chat
      if (chatId.value == cInfo.id) {
        chatItems.removeAllAndNotify {
          // We delete taking into account meta.createdAt to make sure we will not be in situation when two items with the same id will be deleted
          // (it can happen if already deleted chat item in backend still in the list and new one came with the same (re-used) chat item id)
          val remove = it.id == cItem.id && it.meta.createdAt == cItem.meta.createdAt
          if (remove) { AudioPlayer.stop(it) }
          remove
        }
      }
    }

    fun clearChat(rhId: Long?, cInfo: ChatInfo) {
      // clear preview
      val i = getChatIndex(rhId, cInfo.id)
      if (i >= 0) {
        decreaseUnreadCounter(rhId, currentUser.value!!, chats[i].chatStats.unreadCount)
        chats[i] = chats[i].copy(chatItems = arrayListOf(), chatStats = Chat.ChatStats(), chatInfo = cInfo)
      }
      // clear current chat
      if (chatId.value == cInfo.id) {
        chatItemStatuses.clear()
        chatItems.clearAndNotify()
      }
    }

    val popChatCollector = PopChatCollector()

    class PopChatCollector {
      private val subject = MutableSharedFlow<Unit>()
      private var remoteHostId: Long? = null
      private val chatsToPop = mutableMapOf<ChatId, Instant>()

      init {
        withLongRunningApi {
          subject
            .throttleLatest(2000)
            .collect {
              withChats {
                chats.replaceAll(popCollectedChats())
              }
            }
        }
      }

      suspend fun throttlePopChat(rhId: Long?, chatId: ChatId, currentPosition: Int) {
        if (rhId != remoteHostId) {
          chatsToPop.clear()
          remoteHostId = rhId
        }
        if (currentPosition > 0 || chatsToPop.isNotEmpty()) {
          chatsToPop[chatId] = Clock.System.now()
          subject.emit(Unit)
        }
      }

      fun clear() = chatsToPop.clear()

      private fun popCollectedChats(): List<Chat> {
        val chs = mutableListOf<Chat>()
        // collect chats that received updates
        for ((chatId, popTs) in chatsToPop.entries) {
          val ch = getChat(chatId)
          if (ch != null) {
            ch.popTs = popTs
            chs.add(ch)
          }
        }
        // sort chats by pop timestamp in descending order
        val newChats = ArrayList(chs.sortedByDescending { it.popTs })
        newChats.addAll(chats.value.filter { !chatsToPop.containsKey(it.chatInfo.id) } )
        chatsToPop.clear()
        return newChats
      }
    }

    fun markChatItemsRead(remoteHostId: Long?, chatInfo: ChatInfo, itemIds: List<Long>? = null) {
      val cInfo = chatInfo
      val markedRead = markItemsReadInCurrentChat(chatInfo, itemIds)
      // update preview
      val chatIdx = getChatIndex(remoteHostId, cInfo.id)
      if (chatIdx >= 0) {
        val chat = chats[chatIdx]
        val lastId = chat.chatItems.lastOrNull()?.id
        if (lastId != null) {
          val unreadCount = if (itemIds != null) chat.chatStats.unreadCount - markedRead else 0
          decreaseUnreadCounter(remoteHostId, currentUser.value!!, chat.chatStats.unreadCount - unreadCount)
          chats[chatIdx] = chat.copy(
            chatStats = chat.chatStats.copy(unreadCount = unreadCount)
          )
        }
      }
    }

    private fun decreaseCounterInChat(rhId: Long?, chatId: ChatId) {
      val chatIndex = getChatIndex(rhId, chatId)
      if (chatIndex == -1) return

      val chat = chats[chatIndex]
      val unreadCount = kotlin.math.max(chat.chatStats.unreadCount - 1, 0)
      decreaseUnreadCounter(rhId, currentUser.value!!, chat.chatStats.unreadCount - unreadCount)
      chats[chatIndex] = chat.copy(
        chatStats = chat.chatStats.copy(
          unreadCount = unreadCount,
        )
      )
    }

    fun removeChat(rhId: Long?, id: String) {
      val i = getChatIndex(rhId, id)
      if (i != -1) {
        val chat = chats.removeAt(i)
        removeWallpaperFilesFromChat(chat)
      }
    }

    suspend fun upsertGroupMember(rhId: Long?, groupInfo: GroupInfo, member: GroupMember): Boolean {
      // user member was updated
      if (groupInfo.membership.groupMemberId == member.groupMemberId) {
        updateGroup(rhId, groupInfo)
        return false
      }
      // update current chat
      return if (chatId.value == groupInfo.id) {
        val memberIndex = groupMembersIndexes[member.groupMemberId]
        val updated = chatItems.value.map {
          // Take into account only specific changes, not all. Other member updates are not important and can be skipped
          if (it.chatDir is CIDirection.GroupRcv && it.chatDir.groupMember.groupMemberId == member.groupMemberId &&
            (it.chatDir.groupMember.image != member.image ||
                it.chatDir.groupMember.chatViewName != member.chatViewName ||
                it.chatDir.groupMember.blocked != member.blocked ||
                it.chatDir.groupMember.memberRole != member.memberRole)
            )
            it.copy(chatDir = CIDirection.GroupRcv(member))
          else
            it
        }
        if (updated != chatItems.value) {
          chatItems.replaceAll(updated)
        }
        if (memberIndex != null) {
          groupMembers[memberIndex] = member
          false
        } else {
          groupMembers.add(member)
          groupMembersIndexes[member.groupMemberId] = groupMembers.size - 1
          true
        }
      } else {
        false
      }
    }

    suspend fun updateGroupMemberConnectionStats(rhId: Long?, groupInfo: GroupInfo, member: GroupMember, connectionStats: ConnectionStats) {
      val memberConn = member.activeConn
      if (memberConn != null) {
        val updatedConn = memberConn.copy(connectionStats = connectionStats)
        val updatedMember = member.copy(activeConn = updatedConn)
        upsertGroupMember(rhId, groupInfo, updatedMember)
      }
    }
  }

  private fun getChatIndex(rhId: Long?, id: String): Int = chats.value.indexOfFirst { it.id == id && it.remoteHostId == rhId }

  fun updateCurrentUser(rhId: Long?, newProfile: Profile, preferences: FullChatPreferences? = null) {
    val current = currentUser.value ?: return
    val updated = current.copy(
      profile = newProfile.toLocalProfile(current.profile.profileId),
      fullPreferences = preferences ?: current.fullPreferences
    )
    val i = users.indexOfFirst { it.user.userId == current.userId && it.user.remoteHostId == rhId }
    if (i != -1) {
      users[i] = users[i].copy(user = updated)
    }
    currentUser.value = updated
  }

  fun updateCurrentUserUiThemes(rhId: Long?, uiThemes: ThemeModeOverrides?) {
    val current = currentUser.value ?: return
    val updated = current.copy(
      uiThemes = uiThemes
    )
    val i = users.indexOfFirst { it.user.userId == current.userId && it.user.remoteHostId == rhId }
    if (i != -1) {
      users[i] = users[i].copy(user = updated)
    }
    currentUser.value = updated
  }

  suspend fun addLiveDummy(chatInfo: ChatInfo): ChatItem {
    val cItem = ChatItem.liveDummy(chatInfo is ChatInfo.Direct)
    withContext(Dispatchers.Main) {
      chatItems.addAndNotify(cItem)
    }
    return cItem
  }

  fun removeLiveDummy() {
    if (chatItems.value.lastOrNull()?.id == ChatItem.TEMP_LIVE_CHAT_ITEM_ID) {
      chatItems.removeLastAndNotify()
    }
  }

  private fun markItemsReadInCurrentChat(chatInfo: ChatInfo, itemIds: List<Long>? = null): Int {
    val cInfo = chatInfo
    var markedRead = 0
    if (chatId.value == cInfo.id) {
      val items = chatItems.value
      var i = items.lastIndex
      val itemIdsFromRange = itemIds?.toMutableSet() ?: mutableSetOf()
      val markedReadIds = mutableSetOf<Long>()
      while (i >= 0) {
        val item = items[i]
        if (item.meta.itemStatus is CIStatus.RcvNew && (itemIds == null || itemIdsFromRange.contains(item.id))) {
          val newItem = item.withStatus(CIStatus.RcvRead())
          items[i] = newItem
          if (newItem.meta.itemLive != true && newItem.meta.itemTimed?.ttl != null) {
            items[i] = newItem.copy(meta = newItem.meta.copy(itemTimed = newItem.meta.itemTimed.copy(
              deleteAt = Clock.System.now() + newItem.meta.itemTimed.ttl.toDuration(DurationUnit.SECONDS)))
            )
          }
          markedReadIds.add(item.id)
          markedRead++
          if (itemIds != null) {
            itemIdsFromRange.remove(item.id)
            // already set all needed items as read, can finish the loop
            if (itemIdsFromRange.isEmpty()) break
          }
        }
        i--
      }
      chatItemsChangesListener?.read(if (itemIds != null) markedReadIds else null, items)
    }
    return markedRead
  }

  fun increaseUnreadCounter(rhId: Long?, user: UserLike) {
    changeUnreadCounter(rhId, user, 1)
  }

  fun decreaseUnreadCounter(rhId: Long?, user: UserLike, by: Int = 1) {
    changeUnreadCounter(rhId, user, -by)
  }

  private fun changeUnreadCounter(rhId: Long?, user: UserLike, by: Int) {
    val i = users.indexOfFirst { it.user.userId == user.userId && it.user.remoteHostId == rhId }
    if (i != -1) {
      users[i] = users[i].copy(unreadCount = users[i].unreadCount + by)
    }
  }

  fun getChatItemIndexOrNull(cItem: ChatItem): Int? {
    val reversedChatItems = chatItems.asReversed()
    val index = reversedChatItems.indexOfFirst { it.id == cItem.id }
    return if (index != -1) index else null
  }

  // this function analyses "connected" events and assumes that each member will be there only once
  fun getConnectedMemberNames(cItem: ChatItem): Pair<Int, List<String>> {
    var count = 0
    val ns = mutableListOf<String>()
    var idx = getChatItemIndexOrNull(cItem)
    if (cItem.mergeCategory != null && idx != null) {
      val reversedChatItems = chatItems.asReversed()
      while (idx < reversedChatItems.size) {
        val ci = reversedChatItems[idx]
        if (ci.mergeCategory != cItem.mergeCategory) break
        val m = ci.memberConnected
        if (m != null) {
          ns.add(m.displayName)
        }
        count++
        idx++
      }
    }
    return count to ns
  }

  // returns the index of the first item in the same merged group (the first hidden item)
  // and the previous visible item with another merge category
  fun getPrevShownChatItem(ciIndex: Int?, ciCategory: CIMergeCategory?): Pair<Int?, ChatItem?> {
    var i = ciIndex ?: return null to null
    val reversedChatItems = chatItems.asReversed()
    val fst = reversedChatItems.lastIndex
    while (i < fst) {
      i++
      val ci = reversedChatItems[i]
      if (ciCategory == null || ciCategory != ci.mergeCategory) {
        return i - 1 to ci
      }
    }
    return i to null
  }

  // returns the previous member in the same merge group and the count of members in this group
  fun getPrevHiddenMember(member: GroupMember, range: IntRange): Pair<GroupMember?, Int> {
    val reversedChatItems = chatItems.asReversed()
    var prevMember: GroupMember? = null
    val names: MutableSet<Long> = mutableSetOf()
    for (i in range) {
      val dir = reversedChatItems[i].chatDir
      if (dir is CIDirection.GroupRcv) {
        val m = dir.groupMember
        if (prevMember == null && m.groupMemberId != member.groupMemberId) {
          prevMember = m
        }
        names.add(m.groupMemberId)
      }
    }
    return prevMember to names.size
  }

//  func popChat(_ id: String) {
//    if let i = getChatIndex(id) {
//      popChat_(i)
//    }
//  }

  fun replaceConnReqView(id: String, withId: String) {
    if (id == showingInvitation.value?.connId) {
      showingInvitation.value = null
      chatModel.chatItems.clearAndNotify()
      chatModel.chatId.value = withId
      ModalManager.start.closeModals()
      ModalManager.end.closeModals()
    }
  }

  fun dismissConnReqView(id: String) {
    if (id == showingInvitation.value?.connId) {
      showingInvitation.value = null
      chatModel.chatItems.clearAndNotify()
      chatModel.chatId.value = null
      // Close NewChatView
      ModalManager.start.closeModals()
      ModalManager.center.closeModals()
      ModalManager.end.closeModals()
    }
  }

  fun markShowingInvitationUsed() {
    showingInvitation.value = showingInvitation.value?.copy(connChatUsed = true)
  }

  fun setContactNetworkStatus(contact: Contact, status: NetworkStatus) {
    val conn = contact.activeConn
    if (conn != null) {
      networkStatuses[conn.agentConnId] = status
    }
  }

  fun contactNetworkStatus(contact: Contact): NetworkStatus {
    val conn = contact.activeConn
    return if (conn != null)
      networkStatuses[conn.agentConnId] ?: NetworkStatus.Unknown()
    else
      NetworkStatus.Unknown()
  }

  fun addTerminalItem(item: TerminalItem) {
    val maxItems = if (appPreferences.developerTools.get()) 500 else 200
    if (terminalsVisible.isNotEmpty()) {
      withApi {
        addTerminalItem(item, maxItems)
      }
    } else {
      addTerminalItem(item, maxItems)
    }
  }

  private fun addTerminalItem(item: TerminalItem, maxItems: Int) {
    if (terminalItems.value.size >= maxItems) {
      terminalItems.value = terminalItems.value.subList(1, terminalItems.value.size)
    }
    terminalItems.value += item
  }

  val connectedToRemote: Boolean @Composable get() = currentRemoteHost.value != null || remoteCtrlSession.value?.active == true
  fun connectedToRemote(): Boolean = currentRemoteHost.value != null || remoteCtrlSession.value?.active == true
}

interface ChatItemsChangesListener {
  // pass null itemIds if the whole chat now read
  fun read(itemIds: Set<Long>?, newItems: List<ChatItem>)
  fun added(item: Pair<Long, Boolean>, index: Int)
  // itemId, index in old chatModel.chatItems (before the update), isRcvNew (is item unread or not)
  fun removed(itemIds: List<Triple<Long, Int, Boolean>>, newItems: List<ChatItem>)
  fun cleared()
}

data class ShowingInvitation(
  val connId: String,
  val connReq: String,
  val connChatUsed: Boolean,
  val conn: PendingContactConnection
)

enum class ChatType(val type: String) {
  Direct("@"),
  Group("#"),
  Local("*"),
  ContactRequest("<@"),
  ContactConnection(":");
}

@Serializable
data class User(
  val remoteHostId: Long?,
  override val userId: Long,
  val userContactId: Long,
  val localDisplayName: String,
  val profile: LocalProfile,
  val fullPreferences: FullChatPreferences,
  override val activeUser: Boolean,
  val activeOrder: Long,
  override val showNtfs: Boolean,
  val sendRcptsContacts: Boolean,
  val sendRcptsSmallGroups: Boolean,
  val viewPwdHash: UserPwdHash?,
  val uiThemes: ThemeModeOverrides? = null,
): NamedChat, UserLike {
  override val displayName: String get() = profile.displayName
  override val fullName: String get() = profile.fullName
  override val image: String? get() = profile.image
  override val localAlias: String = ""

  val hidden: Boolean = viewPwdHash != null

  val addressShared: Boolean = profile.contactLink != null

  fun updateRemoteHostId(rh: Long?): User =
    if (rh == null) this else this.copy(remoteHostId = rh)

  companion object {
    val sampleData = User(
      remoteHostId = null,
      userId = 1,
      userContactId = 1,
      localDisplayName = "alice",
      profile = LocalProfile.sampleData,
      fullPreferences = FullChatPreferences.sampleData,
      activeUser = true,
      activeOrder = 0,
      showNtfs = true,
      sendRcptsContacts = true,
      sendRcptsSmallGroups = false,
      viewPwdHash = null,
      uiThemes = null,
    )
  }
}

@Serializable
data class UserRef(
  override val userId: Long,
  val localDisplayName: String,
  override val activeUser: Boolean,
  override val showNtfs: Boolean
): UserLike {}

interface UserLike {
  val userId: Long
  val activeUser: Boolean
  val showNtfs: Boolean

  val showNotifications: Boolean get() = activeUser || showNtfs
}

@Serializable
data class UserPwdHash(
  val hash: String,
  val salt: String
)

@Serializable
data class UserInfo(
  val user: User,
  val unreadCount: Int
) {
  companion object {
    val sampleData = UserInfo(
      user = User.sampleData,
      unreadCount = 1
    )
  }
}

typealias ChatId = String

interface NamedChat {
  val displayName: String
  val fullName: String
  val image: String?
  val localAlias: String
  val chatViewName: String
    get() = localAlias.ifEmpty { displayName + (if (fullName == "" || fullName == displayName) "" else " / $fullName") }

  fun anyNameContains(searchAnyCase: String): Boolean {
    val s = searchAnyCase.trim().lowercase()
    return chatViewName.lowercase().contains(s) || displayName.lowercase().contains(s) || fullName.lowercase().contains(s)
  }
}

interface SomeChat {
  val chatType: ChatType
  val localDisplayName: String
  val id: ChatId
  val apiId: Long
  val ready: Boolean
  val chatDeleted: Boolean
  val sendMsgEnabled: Boolean
  val ntfsEnabled: Boolean
  val incognito: Boolean
  fun featureEnabled(feature: ChatFeature): Boolean
  val timedMessagesTTL: Int?
  val createdAt: Instant
  val updatedAt: Instant
}

@Serializable @Stable
data class Chat(
  val remoteHostId: Long?,
  val chatInfo: ChatInfo,
  val chatItems: List<ChatItem>,
  val chatStats: ChatStats = ChatStats()
) {
  @Transient
  var popTs: Instant? = null

  val nextSendGrpInv: Boolean
    get() = when (chatInfo) {
      is ChatInfo.Direct -> chatInfo.contact.nextSendGrpInv
      else -> false
    }

  val userIsObserver: Boolean get() = when(chatInfo) {
    is ChatInfo.Group -> {
      val m = chatInfo.groupInfo.membership
      m.memberActive && m.memberRole == GroupMemberRole.Observer
    }
    else -> false
  }

  val id: String get() = chatInfo.id

  fun groupFeatureEnabled(feature: GroupFeature): Boolean =
    if (chatInfo is ChatInfo.Group) {
      val groupInfo = chatInfo.groupInfo
      val p = groupInfo.fullGroupPreferences
      when (feature) {
        GroupFeature.TimedMessages -> p.timedMessages.on
        GroupFeature.DirectMessages -> p.directMessages.on(groupInfo.membership)
        GroupFeature.FullDelete -> p.fullDelete.on
        GroupFeature.Reactions -> p.reactions.on
        GroupFeature.Voice -> p.voice.on(groupInfo.membership)
        GroupFeature.Files -> p.files.on(groupInfo.membership)
        GroupFeature.SimplexLinks -> p.simplexLinks.on(groupInfo.membership)
        GroupFeature.History -> p.history.on
      }
    } else {
      true
    }

  @Serializable
  data class ChatStats(val unreadCount: Int = 0, val minUnreadItemId: Long = 0, val unreadChat: Boolean = false)

  companion object {
    val sampleData = Chat(
      remoteHostId = null,
      chatInfo = ChatInfo.Direct.sampleData,
      chatItems = arrayListOf(ChatItem.getSampleData())
    )
  }
}

@Serializable
sealed class ChatInfo: SomeChat, NamedChat {

  @Serializable @SerialName("direct")
  data class Direct(val contact: Contact): ChatInfo() {
    override val chatType get() = ChatType.Direct
    override val localDisplayName get() = contact.localDisplayName
    override val id get() = contact.id
    override val apiId get() = contact.apiId
    override val ready get() = contact.ready
    override val chatDeleted get() = contact.chatDeleted
    override val sendMsgEnabled get() = contact.sendMsgEnabled
    override val ntfsEnabled get() = contact.ntfsEnabled
    override val incognito get() = contact.incognito
    override fun featureEnabled(feature: ChatFeature) = contact.featureEnabled(feature)
    override val timedMessagesTTL: Int? get() = contact.timedMessagesTTL
    override val createdAt get() = contact.createdAt
    override val updatedAt get() = contact.updatedAt
    override val displayName get() = contact.displayName
    override val fullName get() = contact.fullName
    override val image get() = contact.image
    override val localAlias: String get() = contact.localAlias
    override fun anyNameContains(searchAnyCase: String): Boolean = contact.anyNameContains(searchAnyCase)

    companion object {
      val sampleData = Direct(Contact.sampleData)
    }
  }

  @Serializable @SerialName("group")
  data class Group(val groupInfo: GroupInfo): ChatInfo() {
    override val chatType get() = ChatType.Group
    override val localDisplayName get() = groupInfo.localDisplayName
    override val id get() = groupInfo.id
    override val apiId get() = groupInfo.apiId
    override val ready get() = groupInfo.ready
    override val chatDeleted get() = groupInfo.chatDeleted
    override val sendMsgEnabled get() = groupInfo.sendMsgEnabled
    override val ntfsEnabled get() = groupInfo.ntfsEnabled
    override val incognito get() = groupInfo.incognito
    override fun featureEnabled(feature: ChatFeature) = groupInfo.featureEnabled(feature)
    override val timedMessagesTTL: Int? get() = groupInfo.timedMessagesTTL
    override val createdAt get() = groupInfo.createdAt
    override val updatedAt get() = groupInfo.updatedAt
    override val displayName get() = groupInfo.displayName
    override val fullName get() = groupInfo.fullName
    override val image get() = groupInfo.image
    override val localAlias get() = groupInfo.localAlias

    companion object {
      val sampleData = Group(GroupInfo.sampleData)
    }
  }

  @Serializable @SerialName("local")
  data class Local(val noteFolder: NoteFolder): ChatInfo() {
    override val chatType get() = ChatType.Local
    override val localDisplayName get() = noteFolder.localDisplayName
    override val id get() = noteFolder.id
    override val apiId get() = noteFolder.apiId
    override val ready get() = noteFolder.ready
    override val chatDeleted get() = noteFolder.chatDeleted
    override val sendMsgEnabled get() = noteFolder.sendMsgEnabled
    override val ntfsEnabled get() = noteFolder.ntfsEnabled
    override val incognito get() = noteFolder.incognito
    override fun featureEnabled(feature: ChatFeature) = noteFolder.featureEnabled(feature)
    override val timedMessagesTTL: Int? get() = noteFolder.timedMessagesTTL
    override val createdAt get() = noteFolder.createdAt
    override val updatedAt get() = noteFolder.updatedAt
    override val displayName get() = noteFolder.displayName
    override val fullName get() = noteFolder.fullName
    override val image get() = noteFolder.image
    override val localAlias get() = noteFolder.localAlias

    companion object {
      val sampleData = Local(NoteFolder.sampleData)
    }
  }

  @Serializable @SerialName("contactRequest")
  class ContactRequest(val contactRequest: UserContactRequest): ChatInfo() {
    override val chatType get() = ChatType.ContactRequest
    override val localDisplayName get() = contactRequest.localDisplayName
    override val id get() = contactRequest.id
    override val apiId get() = contactRequest.apiId
    override val ready get() = contactRequest.ready
    override val chatDeleted get() = contactRequest.chatDeleted
    override val sendMsgEnabled get() = contactRequest.sendMsgEnabled
    override val ntfsEnabled get() = contactRequest.ntfsEnabled
    override val incognito get() = contactRequest.incognito
    override fun featureEnabled(feature: ChatFeature) = contactRequest.featureEnabled(feature)
    override val timedMessagesTTL: Int? get() = contactRequest.timedMessagesTTL
    override val createdAt get() = contactRequest.createdAt
    override val updatedAt get() = contactRequest.updatedAt
    override val displayName get() = contactRequest.displayName
    override val fullName get() = contactRequest.fullName
    override val image get() = contactRequest.image
    override val localAlias get() = contactRequest.localAlias

    companion object {
      val sampleData = ContactRequest(UserContactRequest.sampleData)
    }
  }

  @Serializable @SerialName("contactConnection")
  class ContactConnection(val contactConnection: PendingContactConnection): ChatInfo() {
    override val chatType get() = ChatType.ContactConnection
    override val localDisplayName get() = contactConnection.localDisplayName
    override val id get() = contactConnection.id
    override val apiId get() = contactConnection.apiId
    override val ready get() = contactConnection.ready
    override val chatDeleted get() = contactConnection.chatDeleted
    override val sendMsgEnabled get() = contactConnection.sendMsgEnabled
    override val ntfsEnabled get() = false
    override val incognito get() = contactConnection.incognito
    override fun featureEnabled(feature: ChatFeature) = contactConnection.featureEnabled(feature)
    override val timedMessagesTTL: Int? get() = contactConnection.timedMessagesTTL
    override val createdAt get() = contactConnection.createdAt
    override val updatedAt get() = contactConnection.updatedAt
    override val displayName get() = contactConnection.displayName
    override val fullName get() = contactConnection.fullName
    override val image get() = contactConnection.image
    override val localAlias get() = contactConnection.localAlias

    companion object {
      fun getSampleData(status: ConnStatus = ConnStatus.New, viaContactUri: Boolean = false): ContactConnection =
        ContactConnection(PendingContactConnection.getSampleData(status, viaContactUri))
    }
  }

  @Serializable @SerialName("invalidJSON")
  class InvalidJSON(val json: String): ChatInfo() {
    override val chatType get() = ChatType.Direct
    override val localDisplayName get() = invalidChatName
    override val id get() = ""
    override val apiId get() = 0L
    override val ready get() = false
    override val chatDeleted get() = false
    override val sendMsgEnabled get() = false
    override val ntfsEnabled get() = false
    override val incognito get() = false
    override fun featureEnabled(feature: ChatFeature) = false
    override val timedMessagesTTL: Int? get() = null
    override val createdAt get() = Clock.System.now()
    override val updatedAt get() = Clock.System.now()
    override val displayName get() = invalidChatName
    override val fullName get() = invalidChatName
    override val image get() = null
    override val localAlias get() = ""

    companion object {
      private val invalidChatName = generalGetString(MR.strings.invalid_chat)
    }
  }

  val chatSettings
    get() = when(this) {
      is Direct -> contact.chatSettings
      is Group -> groupInfo.chatSettings
      else -> null
    }

  val chatTs: Instant
    get() = when(this) {
      is Direct -> contact.chatTs ?: contact.updatedAt
      is Group -> groupInfo.chatTs ?: groupInfo.updatedAt
      is Local -> noteFolder.chatTs
      is ContactRequest -> contactRequest.updatedAt
      is ContactConnection -> contactConnection.updatedAt
      is InvalidJSON -> updatedAt
    }

  val userCanSend: Boolean
    get() = when (this) {
      is ChatInfo.Direct -> true
      is ChatInfo.Group -> groupInfo.membership.memberRole >= GroupMemberRole.Member
      is ChatInfo.Local -> true
      else -> false
    }

}

@Serializable
sealed class NetworkStatus {
  val statusString: String get() =
    when (this) {
      is Connected -> generalGetString(MR.strings.server_connected)
      is Error -> generalGetString(MR.strings.server_error)
      else -> generalGetString(MR.strings.server_connecting)
    }
  val statusExplanation: String get() =
    when (this) {
      is Connected -> generalGetString(MR.strings.connected_to_server_to_receive_messages_from_contact)
      is Error -> String.format(generalGetString(MR.strings.trying_to_connect_to_server_to_receive_messages_with_error), connectionError)
      else -> generalGetString(MR.strings.trying_to_connect_to_server_to_receive_messages)
    }

  @Serializable @SerialName("unknown") class Unknown: NetworkStatus()
  @Serializable @SerialName("connected") class Connected: NetworkStatus()
  @Serializable @SerialName("disconnected") class Disconnected: NetworkStatus()
  @Serializable @SerialName("error") class Error(val connectionError: String): NetworkStatus()
}

@Serializable
data class ConnNetworkStatus(val agentConnId: String, val networkStatus: NetworkStatus)

@Serializable
data class Contact(
  val contactId: Long,
  override val localDisplayName: String,
  val profile: LocalProfile,
  val activeConn: Connection? = null,
  val viaGroup: Long? = null,
  val contactUsed: Boolean,
  val contactStatus: ContactStatus,
  val chatSettings: ChatSettings,
  val userPreferences: ChatPreferences,
  val mergedPreferences: ContactUserPreferences,
  override val createdAt: Instant,
  override val updatedAt: Instant,
  val chatTs: Instant?,
  val contactGroupMemberId: Long? = null,
  val contactGrpInvSent: Boolean,
  override val chatDeleted: Boolean,
  val uiThemes: ThemeModeOverrides? = null,
): SomeChat, NamedChat {
  override val chatType get() = ChatType.Direct
  override val id get() = "@$contactId"
  override val apiId get() = contactId
  override val ready get() = activeConn?.connStatus == ConnStatus.Ready
  val sndReady get() = ready || activeConn?.connStatus == ConnStatus.SndReady
  val active get() = contactStatus == ContactStatus.Active
  override val sendMsgEnabled get() = (
      sndReady
          && active
          && !(activeConn?.connectionStats?.ratchetSyncSendProhibited ?: false)
          && !(activeConn?.connDisabled ?: true)
      )
      || nextSendGrpInv
  val nextSendGrpInv get() = contactGroupMemberId != null && !contactGrpInvSent
  override val ntfsEnabled get() = chatSettings.enableNtfs == MsgFilter.All
  override val incognito get() = contactConnIncognito
  override fun featureEnabled(feature: ChatFeature) = when (feature) {
    ChatFeature.TimedMessages -> mergedPreferences.timedMessages.enabled.forUser
    ChatFeature.FullDelete -> mergedPreferences.fullDelete.enabled.forUser
    ChatFeature.Reactions -> mergedPreferences.reactions.enabled.forUser
    ChatFeature.Voice -> mergedPreferences.voice.enabled.forUser
    ChatFeature.Calls -> mergedPreferences.calls.enabled.forUser
  }
  override val timedMessagesTTL: Int? get() = with(mergedPreferences.timedMessages) { if (enabled.forUser) userPreference.pref.ttl else null }
  override val displayName get() = localAlias.ifEmpty { profile.displayName }
  override val fullName get() = profile.fullName
  override val image get() = profile.image
  val contactLink: String? = profile.contactLink
  override val localAlias get() = profile.localAlias
  val verified get() = activeConn?.connectionCode != null

  override fun anyNameContains(searchAnyCase: String): Boolean {
    val s = searchAnyCase.trim().lowercase()
    return profile.chatViewName.lowercase().contains(s) || profile.displayName.lowercase().contains(s) || profile.fullName.lowercase().contains(s)
  }


  val directOrUsed: Boolean get() =
    if (activeConn != null) {
      (activeConn.connLevel == 0 && !activeConn.viaGroupLink) || contactUsed
    } else {
      true
    }

  val contactConnIncognito =
    activeConn?.customUserProfileId != null

  fun allowsFeature(feature: ChatFeature): Boolean = when (feature) {
    ChatFeature.TimedMessages -> mergedPreferences.timedMessages.contactPreference.allow != FeatureAllowed.NO
    ChatFeature.FullDelete -> mergedPreferences.fullDelete.contactPreference.allow != FeatureAllowed.NO
    ChatFeature.Voice -> mergedPreferences.voice.contactPreference.allow != FeatureAllowed.NO
    ChatFeature.Reactions -> mergedPreferences.reactions.contactPreference.allow != FeatureAllowed.NO
    ChatFeature.Calls -> mergedPreferences.calls.contactPreference.allow != FeatureAllowed.NO
  }

  fun userAllowsFeature(feature: ChatFeature): Boolean = when (feature) {
    ChatFeature.TimedMessages -> mergedPreferences.timedMessages.userPreference.pref.allow != FeatureAllowed.NO
    ChatFeature.FullDelete -> mergedPreferences.fullDelete.userPreference.pref.allow != FeatureAllowed.NO
    ChatFeature.Reactions -> mergedPreferences.reactions.userPreference.pref.allow != FeatureAllowed.NO
    ChatFeature.Voice -> mergedPreferences.voice.userPreference.pref.allow != FeatureAllowed.NO
    ChatFeature.Calls -> mergedPreferences.calls.userPreference.pref.allow != FeatureAllowed.NO
  }

  companion object {
    val sampleData = Contact(
      contactId = 1,
      localDisplayName = "alice",
      profile = LocalProfile.sampleData,
      activeConn = Connection.sampleData,
      contactUsed = true,
      contactStatus = ContactStatus.Active,
      chatSettings = ChatSettings(enableNtfs = MsgFilter.All, sendRcpts = null, favorite = false),
      userPreferences = ChatPreferences.sampleData,
      mergedPreferences = ContactUserPreferences.sampleData,
      createdAt = Clock.System.now(),
      updatedAt = Clock.System.now(),
      chatTs = Clock.System.now(),
      contactGrpInvSent = false,
      chatDeleted = false,
      uiThemes = null,
    )
  }
}

@Serializable
data class NavigationInfo(
  val afterUnread: Int = 0,
  val afterTotal: Int = 0
)

@Serializable
enum class ContactStatus {
  @SerialName("active") Active,
  @SerialName("deleted") Deleted,
  @SerialName("deletedByUser") DeletedByUser;
}

@Serializable
class ContactRef(
  val contactId: Long,
  val agentConnId: String,
  val connId: Long,
  var localDisplayName: String
) {
  val id: ChatId get() = "@$contactId"
}

@Serializable
class ContactSubStatus(
  val contact: Contact,
  val contactError: ChatError? = null
)

@Serializable
data class Connection(
  val connId: Long,
  val agentConnId: String,
  val peerChatVRange: VersionRange,
  val connStatus: ConnStatus,
  val connLevel: Int,
  val viaGroupLink: Boolean,
  val customUserProfileId: Long? = null,
  val connectionCode: SecurityCode? = null,
  val pqSupport: Boolean,
  val pqEncryption: Boolean,
  val pqSndEnabled: Boolean? = null,
  val pqRcvEnabled: Boolean? = null,
  val connectionStats: ConnectionStats? = null,
  val authErrCounter: Int,
  val quotaErrCounter: Int
) {
  val id: ChatId get() = ":$connId"

  val connDisabled: Boolean
    get() = authErrCounter >= 10 // authErrDisableCount in core

  val connInactive: Boolean
    get() = quotaErrCounter >= 5 // quotaErrInactiveCount in core

  val connPQEnabled: Boolean
    get() = pqSndEnabled == true && pqRcvEnabled == true

  companion object {
    val sampleData = Connection(connId = 1, agentConnId = "abc", connStatus = ConnStatus.Ready, connLevel = 0, viaGroupLink = false, peerChatVRange = VersionRange(1, 1), customUserProfileId = null, pqSupport = false, pqEncryption = false, authErrCounter = 0, quotaErrCounter = 0)
  }
}

@Serializable
data class VersionRange(val minVersion: Int, val maxVersion: Int)

@Serializable
data class SecurityCode(val securityCode: String, val verifiedAt: Instant)

@Serializable
data class Profile(
  override val displayName: String,
  override val fullName: String,
  override val image: String? = null,
  override val localAlias : String = "",
  val contactLink: String? = null,
  val preferences: ChatPreferences? = null
): NamedChat {
  val profileViewName: String
    get() {
      return if (fullName == "" || displayName == fullName) displayName else "$displayName ($fullName)"
    }

  fun toLocalProfile(profileId: Long): LocalProfile = LocalProfile(profileId, displayName, fullName, image, localAlias, contactLink, preferences)

  companion object {
    val sampleData = Profile(
      displayName = "alice",
      fullName = "Alice"
    )
  }
}

@Serializable
data class LocalProfile(
  val profileId: Long,
  override val displayName: String,
  override val fullName: String,
  override val image: String? = null,
  override val localAlias: String,
  val contactLink: String? = null,
  val preferences: ChatPreferences? = null
): NamedChat {
  val profileViewName: String = localAlias.ifEmpty { if (fullName == "" || displayName == fullName) displayName else "$displayName ($fullName)" }

  fun toProfile(): Profile = Profile(displayName, fullName, image, localAlias, contactLink, preferences)

  companion object {
    val sampleData = LocalProfile(
      profileId = 1L,
      displayName = "alice",
      fullName = "Alice",
      preferences = ChatPreferences.sampleData,
      localAlias = ""
    )
  }
}

@Serializable
data class UserProfileUpdateSummary(
  val updateSuccesses: Int,
  val updateFailures: Int,
  val changedContacts: List<Contact>
)

@Serializable
class Group (
  val groupInfo: GroupInfo,
  var members: List<GroupMember>
)

@Serializable
sealed class ForwardConfirmation {
  @Serializable @SerialName("filesNotAccepted") data class FilesNotAccepted(val fileIds: List<Long>) : ForwardConfirmation()
  @Serializable @SerialName("filesInProgress") data class FilesInProgress(val filesCount: Int) : ForwardConfirmation()
  @Serializable @SerialName("filesMissing") data class FilesMissing(val filesCount: Int) : ForwardConfirmation()
  @Serializable @SerialName("filesFailed") data class FilesFailed(val filesCount: Int) : ForwardConfirmation()
}

@Serializable
data class GroupInfo (
  val groupId: Long,
  override val localDisplayName: String,
  val groupProfile: GroupProfile,
  val businessChat: BusinessChatInfo? = null,
  val fullGroupPreferences: FullGroupPreferences,
  val membership: GroupMember,
  val hostConnCustomUserProfileId: Long? = null,
  val chatSettings: ChatSettings,
  override val createdAt: Instant,
  override val updatedAt: Instant,
  val chatTs: Instant?,
  val uiThemes: ThemeModeOverrides? = null,
): SomeChat, NamedChat {
  override val chatType get() = ChatType.Group
  override val id get() = "#$groupId"
  override val apiId get() = groupId
  override val ready get() = membership.memberActive
  override val chatDeleted get() = false
  override val sendMsgEnabled get() = membership.memberActive
  override val ntfsEnabled get() = chatSettings.enableNtfs == MsgFilter.All
  override val incognito get() = membership.memberIncognito
  override fun featureEnabled(feature: ChatFeature) = when (feature) {
    ChatFeature.TimedMessages -> fullGroupPreferences.timedMessages.on
    ChatFeature.FullDelete -> fullGroupPreferences.fullDelete.on
    ChatFeature.Reactions -> fullGroupPreferences.reactions.on
    ChatFeature.Voice -> fullGroupPreferences.voice.on(membership)
    ChatFeature.Calls -> false
  }
  override val timedMessagesTTL: Int? get() = with(fullGroupPreferences.timedMessages) { if (on) ttl else null }
  override val displayName get() = groupProfile.displayName
  override val fullName get() = groupProfile.fullName
  override val image get() = groupProfile.image
  override val localAlias get() = ""

  val isOwner: Boolean
    get() = membership.memberRole == GroupMemberRole.Owner && membership.memberCurrent

  val canDelete: Boolean
    get() = membership.memberRole == GroupMemberRole.Owner || !membership.memberCurrent

  val canAddMembers: Boolean
    get() = membership.memberRole >= GroupMemberRole.Admin && membership.memberActive

  companion object {
    val sampleData = GroupInfo(
      groupId = 1,
      localDisplayName = "team",
      groupProfile = GroupProfile.sampleData,
      fullGroupPreferences = FullGroupPreferences.sampleData,
      membership = GroupMember.sampleData,
      hostConnCustomUserProfileId = null,
      chatSettings = ChatSettings(enableNtfs = MsgFilter.All, sendRcpts = null, favorite = false),
      createdAt = Clock.System.now(),
      updatedAt = Clock.System.now(),
      chatTs = Clock.System.now(),
      uiThemes = null,
    )
  }
}

@Serializable
data class GroupRef(val groupId: Long, val localDisplayName: String)

@Serializable
data class GroupProfile (
  override val displayName: String,
  override val fullName: String,
  val description: String? = null,
  override val image: String? = null,
  override val localAlias: String = "",
  val groupPreferences: GroupPreferences? = null
): NamedChat {
  companion object {
    val sampleData = GroupProfile(
      displayName = "team",
      fullName = "My Team"
    )
  }
}

@Serializable
data class BusinessChatInfo (
  val chatType: BusinessChatType,
  val businessId: String,
  val customerId: String,
)

@Serializable
enum class BusinessChatType {
  @SerialName("business") Business,
  @SerialName("customer") Customer,
}

@Serializable
data class GroupMember (
  val groupMemberId: Long,
  val groupId: Long,
  val memberId: String,
  val memberRole: GroupMemberRole,
  val memberCategory: GroupMemberCategory,
  val memberStatus: GroupMemberStatus,
  val memberSettings: GroupMemberSettings,
  val blockedByAdmin: Boolean,
  val invitedBy: InvitedBy,
  val localDisplayName: String,
  val memberProfile: LocalProfile,
  val memberContactId: Long? = null,
  val memberContactProfileId: Long,
  var activeConn: Connection? = null
): NamedChat {
  val id: String get() = "#$groupId @$groupMemberId"
  override val displayName: String
    get() {
      val p = memberProfile
      val name = p.localAlias.ifEmpty { p.displayName }
      return pastMember(name)
    }
  override val fullName: String get() = memberProfile.fullName
  override val image: String? get() = memberProfile.image
  val contactLink: String? = memberProfile.contactLink
  val verified get() = activeConn?.connectionCode != null
  val blocked get() = blockedByAdmin || !memberSettings.showMessages

  override val localAlias: String = memberProfile.localAlias

  override val chatViewName: String
    get() {
      val p = memberProfile
      val name = p.localAlias.ifEmpty { p.displayName + (if (p.fullName == "" || p.fullName == p.displayName) "" else " / ${p.fullName}") }
      return pastMember(name)
    }

  private fun pastMember(name: String): String {
    return if (memberStatus == GroupMemberStatus.MemUnknown)
      String.format(generalGetString(MR.strings.past_member_vName), name)
    else
      name
  }

  val memberActive: Boolean get() = when (this.memberStatus) {
    GroupMemberStatus.MemRemoved -> false
    GroupMemberStatus.MemLeft -> false
    GroupMemberStatus.MemGroupDeleted -> false
    GroupMemberStatus.MemUnknown -> false
    GroupMemberStatus.MemInvited -> false
    GroupMemberStatus.MemIntroduced -> false
    GroupMemberStatus.MemIntroInvited -> false
    GroupMemberStatus.MemAccepted -> false
    GroupMemberStatus.MemAnnounced -> false
    GroupMemberStatus.MemConnected -> true
    GroupMemberStatus.MemComplete -> true
    GroupMemberStatus.MemCreator -> true
  }

  val memberCurrent: Boolean get() = when (this.memberStatus) {
    GroupMemberStatus.MemRemoved -> false
    GroupMemberStatus.MemLeft -> false
    GroupMemberStatus.MemGroupDeleted -> false
    GroupMemberStatus.MemUnknown -> false
    GroupMemberStatus.MemInvited -> false
    GroupMemberStatus.MemIntroduced -> true
    GroupMemberStatus.MemIntroInvited -> true
    GroupMemberStatus.MemAccepted -> true
    GroupMemberStatus.MemAnnounced -> true
    GroupMemberStatus.MemConnected -> true
    GroupMemberStatus.MemComplete -> true
    GroupMemberStatus.MemCreator -> true
  }

  fun canBeRemoved(groupInfo: GroupInfo): Boolean {
    val userRole = groupInfo.membership.memberRole
    return memberStatus != GroupMemberStatus.MemRemoved && memberStatus != GroupMemberStatus.MemLeft
        && userRole >= GroupMemberRole.Admin && userRole >= memberRole && groupInfo.membership.memberActive
  }

  fun canChangeRoleTo(groupInfo: GroupInfo): List<GroupMemberRole>? =
    if (!canBeRemoved(groupInfo)) null
    else groupInfo.membership.memberRole.let { userRole ->
      GroupMemberRole.selectableRoles.filter { it <= userRole }
    }

  fun canBlockForAll(groupInfo: GroupInfo): Boolean {
    val userRole = groupInfo.membership.memberRole
    return memberStatus != GroupMemberStatus.MemRemoved && memberStatus != GroupMemberStatus.MemLeft && memberRole < GroupMemberRole.Admin
        && userRole >= GroupMemberRole.Admin && userRole >= memberRole && groupInfo.membership.memberActive
  }

  val memberIncognito = memberProfile.profileId != memberContactProfileId

  companion object {
    val sampleData = GroupMember(
      groupMemberId = 1,
      groupId = 1,
      memberId = "abcd",
      memberRole = GroupMemberRole.Member,
      memberCategory = GroupMemberCategory.InviteeMember,
      memberStatus = GroupMemberStatus.MemComplete,
      memberSettings = GroupMemberSettings(showMessages = true),
      blockedByAdmin = false,
      invitedBy = InvitedBy.IBUser(),
      localDisplayName = "alice",
      memberProfile = LocalProfile.sampleData,
      memberContactId = 1,
      memberContactProfileId = 1L,
      activeConn = Connection.sampleData
    )
  }
}

@Serializable
data class GroupMemberSettings(val showMessages: Boolean) {}

@Serializable
data class GroupMemberRef(
  val groupMemberId: Long,
  val profile: Profile
)

@Serializable
data class GroupMemberIds(
  val groupMemberId: Long,
  val groupId: Long
)

@Serializable
enum class GroupMemberRole(val memberRole: String) {
  @SerialName("observer") Observer("observer"), // order matters in comparisons
  @SerialName("author") Author("author"),
  @SerialName("member") Member("member"),
  @SerialName("moderator") Moderator("moderator"),
  @SerialName("admin") Admin("admin"),
  @SerialName("owner") Owner("owner");

  companion object {
    val selectableRoles: List<GroupMemberRole> = listOf(Observer, Member, Admin, Owner)
  }

  val text: String get() = when (this) {
    Observer -> generalGetString(MR.strings.group_member_role_observer)
    Author -> generalGetString(MR.strings.group_member_role_author)
    Member -> generalGetString(MR.strings.group_member_role_member)
    Moderator -> generalGetString(MR.strings.group_member_role_moderator)
    Admin -> generalGetString(MR.strings.group_member_role_admin)
    Owner -> generalGetString(MR.strings.group_member_role_owner)
  }
}

@Serializable
enum class GroupMemberCategory {
  @SerialName("user") UserMember,
  @SerialName("invitee") InviteeMember,
  @SerialName("host") HostMember,
  @SerialName("pre") PreMember,
  @SerialName("post") PostMember;
}

@Serializable
enum class GroupMemberStatus {
  @SerialName("removed") MemRemoved,
  @SerialName("left") MemLeft,
  @SerialName("deleted") MemGroupDeleted,
  @SerialName("unknown") MemUnknown,
  @SerialName("invited") MemInvited,
  @SerialName("introduced") MemIntroduced,
  @SerialName("intro-inv") MemIntroInvited,
  @SerialName("accepted") MemAccepted,
  @SerialName("announced") MemAnnounced,
  @SerialName("connected") MemConnected,
  @SerialName("complete") MemComplete,
  @SerialName("creator") MemCreator;

  val text: String get() = when (this) {
    MemRemoved -> generalGetString(MR.strings.group_member_status_removed)
    MemLeft -> generalGetString(MR.strings.group_member_status_left)
    MemGroupDeleted -> generalGetString(MR.strings.group_member_status_group_deleted)
    MemUnknown -> generalGetString(MR.strings.group_member_status_unknown)
    MemInvited -> generalGetString(MR.strings.group_member_status_invited)
    MemIntroduced -> generalGetString(MR.strings.group_member_status_introduced)
    MemIntroInvited -> generalGetString(MR.strings.group_member_status_intro_invitation)
    MemAccepted -> generalGetString(MR.strings.group_member_status_accepted)
    MemAnnounced -> generalGetString(MR.strings.group_member_status_announced)
    MemConnected -> generalGetString(MR.strings.group_member_status_connected)
    MemComplete -> generalGetString(MR.strings.group_member_status_complete)
    MemCreator -> generalGetString(MR.strings.group_member_status_creator)
  }

  val shortText: String get() = when (this) {
    MemRemoved -> generalGetString(MR.strings.group_member_status_removed)
    MemLeft -> generalGetString(MR.strings.group_member_status_left)
    MemGroupDeleted -> generalGetString(MR.strings.group_member_status_group_deleted)
    MemUnknown -> generalGetString(MR.strings.group_member_status_unknown_short)
    MemInvited -> generalGetString(MR.strings.group_member_status_invited)
    MemIntroduced -> generalGetString(MR.strings.group_member_status_connecting)
    MemIntroInvited -> generalGetString(MR.strings.group_member_status_connecting)
    MemAccepted -> generalGetString(MR.strings.group_member_status_connecting)
    MemAnnounced -> generalGetString(MR.strings.group_member_status_connecting)
    MemConnected -> generalGetString(MR.strings.group_member_status_connected)
    MemComplete -> generalGetString(MR.strings.group_member_status_complete)
    MemCreator -> generalGetString(MR.strings.group_member_status_creator)
  }
}

@Serializable
sealed class InvitedBy {
  @Serializable @SerialName("contact") class IBContact(val byContactId: Long): InvitedBy()
  @Serializable @SerialName("user") class IBUser: InvitedBy()
  @Serializable @SerialName("unknown") class IBUnknown: InvitedBy()
}

@Serializable
class LinkPreview (
  val uri: String,
  val title: String,
  val description: String,
  val image: String
) {
  companion object {
    val sampleData = LinkPreview(
      uri = "https://www.duckduckgo.com",
      title = "Privacy, simplified.",
      description = "The Internet privacy company that empowers you to seamlessly take control of your personal information online, without any tradeoffs.",
      image = "data:image/jpg;base64,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"
    )
  }
}

@Serializable
class MemberSubError (
  val member: GroupMemberIds,
  val memberError: ChatError
)

@Serializable
class NoteFolder(
  val noteFolderId: Long,
  val favorite: Boolean,
  val unread: Boolean,
  override val createdAt: Instant,
  override val updatedAt: Instant,
  val chatTs: Instant
): SomeChat, NamedChat {
  override val chatType get() = ChatType.Local
  override val id get() = "*$noteFolderId"
  override val apiId get() = noteFolderId
  override val chatDeleted get() = false
  override val ready get() = true
  override val sendMsgEnabled get() = true
  override val ntfsEnabled get() = false
  override val incognito get() = false
  override fun featureEnabled(feature: ChatFeature) = feature == ChatFeature.Voice
  override val timedMessagesTTL: Int? get() = null
  override val displayName get() = generalGetString(MR.strings.note_folder_local_display_name)
  override val fullName get() = ""
  override val image get() = null
  override val localAlias get() = ""
  override val localDisplayName: String get() = ""

  companion object {
    val sampleData = NoteFolder(
      noteFolderId = 1,
      favorite = false,
      unread = false,
      createdAt = Clock.System.now(),
      updatedAt = Clock.System.now(),
      chatTs = Clock.System.now()
    )
  }
}

@Serializable
class UserContactRequest (
  val contactRequestId: Long,
  val cReqChatVRange: VersionRange,
  override val localDisplayName: String,
  val profile: Profile,
  override val createdAt: Instant,
  override val updatedAt: Instant
): SomeChat, NamedChat {
  override val chatType get() = ChatType.ContactRequest
  override val id get() = "<@$contactRequestId"
  override val apiId get() = contactRequestId
  override val chatDeleted get() = false
  override val ready get() = true
  override val sendMsgEnabled get() = false
  override val ntfsEnabled get() = false
  override val incognito get() = false
  override fun featureEnabled(feature: ChatFeature) = false
  override val timedMessagesTTL: Int? get() = null
  override val displayName get() = profile.displayName
  override val fullName get() = profile.fullName
  override val image get() = profile.image
  override val localAlias get() = ""

  companion object {
    val sampleData = UserContactRequest(
      contactRequestId = 1,
      cReqChatVRange = VersionRange(1, 1),
      localDisplayName = "alice",
      profile = Profile.sampleData,
      createdAt = Clock.System.now(),
      updatedAt = Clock.System.now()
    )
  }
}

@Serializable
class PendingContactConnection(
  val pccConnId: Long,
  val pccAgentConnId: String,
  val pccConnStatus: ConnStatus,
  val viaContactUri: Boolean,
  val groupLinkId: String? = null,
  val customUserProfileId: Long? = null,
  val connReqInv: String? = null,
  override val localAlias: String,
  override val createdAt: Instant,
  override val updatedAt: Instant
): SomeChat, NamedChat {
  override val chatType get() = ChatType.ContactConnection
  override val id get () = ":$pccConnId"
  override val apiId get() = pccConnId
  override val chatDeleted get() = false
  override val ready get() = false
  override val sendMsgEnabled get() = false
  override val ntfsEnabled get() = false
  override val incognito get() = customUserProfileId != null
  override fun featureEnabled(feature: ChatFeature) = false
  override val timedMessagesTTL: Int? get() = null
  override val localDisplayName get() = String.format(generalGetString(MR.strings.connection_local_display_name), pccConnId)
  override val displayName: String get() {
    if (localAlias.isNotEmpty()) return localAlias
    val initiated = pccConnStatus.initiated
    return if (initiated == null) {
      // this should not be in the chat list
      generalGetString(MR.strings.display_name_connection_established)
    } else {
      generalGetString(
        if (viaContactUri) MR.strings.display_name_requested_to_connect
        else if (initiated) MR.strings.display_name_invited_to_connect
        else MR.strings.display_name_accepted_invitation
      )
    }
  }
  override val fullName get() = ""
  override val image get() = null

  val initiated get() = (pccConnStatus.initiated ?: false) && !viaContactUri

  val description: String get() {
    val initiated = pccConnStatus.initiated
    return if (initiated == null) "" else generalGetString(
      if (initiated && !viaContactUri)
        if (incognito) MR.strings.description_you_shared_one_time_link_incognito else MR.strings.description_you_shared_one_time_link
      else if (viaContactUri)
        if (groupLinkId != null)
          if (incognito) MR.strings.description_via_group_link_incognito else MR.strings.description_via_group_link
        else
          if (incognito) MR.strings.description_via_contact_address_link_incognito else MR.strings.description_via_contact_address_link
      else
        if (incognito) MR.strings.description_via_one_time_link_incognito else MR.strings.description_via_one_time_link
    )
  }

  companion object {
    fun getSampleData(status: ConnStatus = ConnStatus.New, viaContactUri: Boolean = false): PendingContactConnection =
      PendingContactConnection(
        pccConnId = 1,
        pccAgentConnId = "abcd",
        pccConnStatus = status,
        viaContactUri = viaContactUri,
        localAlias = "",
        customUserProfileId = null,
        createdAt = Clock.System.now(),
        updatedAt = Clock.System.now()
      )
  }
}

@Serializable
enum class ConnStatus {
  @SerialName("new") New,
  @SerialName("prepared") Prepared,
  @SerialName("joined") Joined,
  @SerialName("requested") Requested,
  @SerialName("accepted") Accepted,
  @SerialName("snd-ready") SndReady,
  @SerialName("ready") Ready,
  @SerialName("deleted") Deleted;

  val initiated: Boolean? get() = when (this) {
    New -> true
    Prepared -> false
    Joined -> false
    Requested -> true
    Accepted -> true
    SndReady -> null
    Ready -> null
    Deleted -> null
  }
}

@Serializable
data class ChatItemDeletion (
  val deletedChatItem: AChatItem,
  val toChatItem: AChatItem? = null
)

@Serializable
class AChatItem (
  val chatInfo: ChatInfo,
  val chatItem: ChatItem
)

@Serializable
class ACIReaction(
  val chatInfo: ChatInfo,
  val chatReaction: CIReaction
)

@Serializable
data class MemberReaction(
  val groupMember: GroupMember,
  val reactionTs: Instant
)

@Serializable
class CIReaction(
  val chatDir: CIDirection,
  val chatItem: ChatItem,
  val sentAt: Instant,
  val reaction: MsgReaction
)

@Serializable @Stable
data class ChatItem (
  val chatDir: CIDirection,
  val meta: CIMeta,
  val content: CIContent,
  val formattedText: List<FormattedText>? = null,
  val quotedItem: CIQuote? = null,
  val reactions: List<CIReactionCount>,
  val file: CIFile? = null
) {
  val id: Long get() = meta.itemId
  val timestampText: String get() = meta.timestampText

  val text: String get() {
    val mc = content.msgContent
    return when {
      content.text == "" && file != null && mc is MsgContent.MCVoice -> String.format(generalGetString(MR.strings.voice_message_with_duration), durationText(mc.duration))
      content.text == "" && file != null -> file.fileName
      else -> content.text
    }
  }

  val isRcvNew: Boolean get() = meta.isRcvNew

  val allowAddReaction: Boolean get() =
    meta.itemDeleted == null && !isLiveDummy && (reactions.count { it.userReacted } < 3)

  val isLiveDummy: Boolean get() = meta.itemId == TEMP_LIVE_CHAT_ITEM_ID

  val encryptedFile: Boolean? = if (file?.fileSource == null) null else file.fileSource.cryptoArgs != null

  val memberDisplayName: String? get() =
    when (chatDir) {
      is CIDirection.GroupRcv -> when (content) {
        is CIContent.RcvGroupEventContent -> when (val event = content.rcvGroupEvent) {
          is RcvGroupEvent.MemberProfileUpdated -> {
            val to = event.toProfile
            val from = event.fromProfile
            when {
              to.displayName != from.displayName || to.fullName != from.fullName -> null
              else -> chatDir.groupMember.chatViewName
            }
          }

          else -> chatDir.groupMember.chatViewName
        }

        else -> chatDir.groupMember.chatViewName
      }

      else -> null
    }

  val localNote: Boolean = chatDir is CIDirection.LocalSnd || chatDir is CIDirection.LocalRcv

  val isDeletedContent: Boolean get() =
    when (content) {
      is CIContent.SndDeleted -> true
      is CIContent.RcvDeleted -> true
      is CIContent.SndModerated -> true
      is CIContent.RcvModerated -> true
      is CIContent.RcvBlocked -> true
      else -> false
    }

  val memberConnected: GroupMember? get() =
    when (chatDir) {
      is CIDirection.GroupRcv -> when (content) {
        is CIContent.RcvGroupEventContent -> when (content.rcvGroupEvent) {
          is RcvGroupEvent.MemberConnected -> chatDir.groupMember
          else -> null
        }
        else -> null
      }
      else -> null
    }

  val mergeCategory: CIMergeCategory?
    get() = when (content) {
      is CIContent.RcvChatFeature,
      is CIContent.SndChatFeature,
      is CIContent.RcvGroupFeature,
      is CIContent.SndGroupFeature -> CIMergeCategory.ChatFeature
      is CIContent.RcvGroupEventContent -> when (content.rcvGroupEvent) {
        is RcvGroupEvent.UserRole, is RcvGroupEvent.UserDeleted, is RcvGroupEvent.GroupDeleted, is RcvGroupEvent.MemberCreatedContact -> null
        else -> CIMergeCategory.RcvGroupEvent
      }
      is CIContent.SndGroupEventContent -> when (content.sndGroupEvent) {
        is SndGroupEvent.UserRole, is SndGroupEvent.UserLeft -> null
        else -> CIMergeCategory.SndGroupEvent
      }
      else -> {
        if (meta.itemDeleted == null) {
          null
        } else {
          if (chatDir.sent) CIMergeCategory.SndItemDeleted else CIMergeCategory.RcvItemDeleted
        }
      }
    }

  fun memberToModerate(chatInfo: ChatInfo): Pair<GroupInfo, GroupMember?>? {
    return if (chatInfo is ChatInfo.Group && chatDir is CIDirection.GroupRcv) {
      val m = chatInfo.groupInfo.membership
      if (m.memberRole >= GroupMemberRole.Admin && m.memberRole >= chatDir.groupMember.memberRole && meta.itemDeleted == null) {
        chatInfo.groupInfo to chatDir.groupMember
      } else {
      null
      }
    } else if (chatInfo is ChatInfo.Group && chatDir is CIDirection.GroupSnd) {
      val m = chatInfo.groupInfo.membership
      if (m.memberRole >= GroupMemberRole.Admin) {
        chatInfo.groupInfo to null
      } else {
        null
      }
    } else {
      null
    }
  }

  val showLocalDelete: Boolean
    get() = when (content) {
      is CIContent.SndDirectE2EEInfo -> false
      is CIContent.RcvDirectE2EEInfo -> false
      is CIContent.SndGroupE2EEInfo -> false
      is CIContent.RcvGroupE2EEInfo -> false
      else -> true
    }

  val isReport: Boolean get() = when (content) {
    is CIContent.SndMsgContent, is CIContent.RcvMsgContent ->
      content.msgContent is MsgContent.MCReport
    else -> false
  }

  val canBeDeletedForSelf: Boolean
    get() = (content.msgContent != null && !meta.isLive) || meta.itemDeleted != null || isDeletedContent || mergeCategory != null || showLocalDelete

  val showNotification: Boolean get() =
    when (content) {
      is CIContent.SndMsgContent -> false
      is CIContent.RcvMsgContent -> meta.itemDeleted == null
      is CIContent.SndDeleted -> false
      is CIContent.RcvDeleted -> false
      is CIContent.SndCall -> false
      is CIContent.RcvCall -> false // notification is shown on CallInvitation instead
      is CIContent.RcvIntegrityError -> false
      is CIContent.RcvDecryptionError -> false
      is CIContent.RcvGroupInvitation -> true
      is CIContent.SndGroupInvitation -> false
      is CIContent.RcvDirectEventContent -> when (content.rcvDirectEvent) {
        is RcvDirectEvent.ContactDeleted -> false
        is RcvDirectEvent.ProfileUpdated -> false
      }
      is CIContent.RcvGroupEventContent -> when (content.rcvGroupEvent) {
        is RcvGroupEvent.MemberAdded -> false
        is RcvGroupEvent.MemberConnected -> false
        is RcvGroupEvent.MemberLeft -> false
        is RcvGroupEvent.MemberRole -> false
        is RcvGroupEvent.MemberBlocked -> false
        is RcvGroupEvent.UserRole -> true
        is RcvGroupEvent.MemberDeleted -> false
        is RcvGroupEvent.UserDeleted -> true
        is RcvGroupEvent.GroupDeleted -> true
        is RcvGroupEvent.GroupUpdated -> false
        is RcvGroupEvent.InvitedViaGroupLink -> false
        is RcvGroupEvent.MemberCreatedContact -> false
        is RcvGroupEvent.MemberProfileUpdated -> false
      }
      is CIContent.SndGroupEventContent -> false
      is CIContent.RcvConnEventContent -> false
      is CIContent.SndConnEventContent -> false
      is CIContent.RcvChatFeature -> false
      is CIContent.SndChatFeature -> false
      is CIContent.RcvChatPreference -> false
      is CIContent.SndChatPreference -> false
      is CIContent.RcvGroupFeature -> false
      is CIContent.SndGroupFeature -> false
      is CIContent.RcvChatFeatureRejected -> true
      is CIContent.RcvGroupFeatureRejected -> false
      is CIContent.SndModerated -> false
      is CIContent.RcvModerated -> false
      is CIContent.RcvBlocked -> false
      is CIContent.SndDirectE2EEInfo -> false
      is CIContent.RcvDirectE2EEInfo -> false
      is CIContent.SndGroupE2EEInfo -> false
      is CIContent.RcvGroupE2EEInfo -> false
      is CIContent.InvalidJSON -> false
    }

  fun withStatus(status: CIStatus): ChatItem = this.copy(meta = meta.copy(itemStatus = status))

  companion object {
    fun getSampleData(
      id: Long = 1,
      dir: CIDirection = CIDirection.DirectSnd(),
      ts: Instant = Clock.System.now(),
      text: String = "hello\nthere",
      status: CIStatus = CIStatus.SndNew(),
      sentViaProxy: Boolean? = null,
      quotedItem: CIQuote? = null,
      file: CIFile? = null,
      itemForwarded: CIForwardedFrom? = null,
      itemDeleted: CIDeleted? = null,
      itemEdited: Boolean = false,
      itemTimed: CITimed? = null,
      itemLive: Boolean = false,
      deletable: Boolean = true,
      editable: Boolean = true
    ) =
      ChatItem(
        chatDir = dir,
        meta = CIMeta.getSample(id, ts, text, status, sentViaProxy, itemForwarded, itemDeleted, itemEdited, itemTimed, itemLive, deletable, editable),
        content = CIContent.SndMsgContent(msgContent = MsgContent.MCText(text)),
        quotedItem = quotedItem,
        reactions = listOf(),
        file = file
      )

    fun getFileMsgContentSample(
      id: Long = 1,
      text: String = "",
      fileName: String = "test.txt",
      fileSize: Long = 100,
      fileStatus: CIFileStatus = CIFileStatus.RcvComplete
    ) =
      ChatItem(
        chatDir = CIDirection.DirectRcv(),
        meta = CIMeta.getSample(id, Clock.System.now(), text, CIStatus.RcvRead()),
        content = CIContent.RcvMsgContent(msgContent = MsgContent.MCFile(text)),
        quotedItem = null,
        reactions = listOf(),
        file = CIFile.getSample(fileName = fileName, fileSize = fileSize, fileStatus = fileStatus)
      )

    fun getDeletedContentSampleData(
      id: Long = 1,
      dir: CIDirection = CIDirection.DirectRcv(),
      ts: Instant = Clock.System.now(),
      text: String = "this item is deleted", // sample not localized
      status: CIStatus = CIStatus.RcvRead()
    ) =
      ChatItem(
        chatDir = dir,
        meta = CIMeta.getSample(id, ts, text, status),
        content = CIContent.RcvDeleted(deleteMode = CIDeleteMode.cidmBroadcast),
        quotedItem = null,
        reactions = listOf(),
        file = null
      )

    fun getGroupInvitationSample(status: CIGroupInvitationStatus = CIGroupInvitationStatus.Pending) =
      ChatItem(
        chatDir = CIDirection.DirectRcv(),
        meta = CIMeta.getSample(1, Clock.System.now(), "received invitation to join group team as admin", CIStatus.RcvRead()),
        content = CIContent.RcvGroupInvitation(groupInvitation = CIGroupInvitation.getSample(status = status), memberRole = GroupMemberRole.Admin),
        quotedItem = null,
        reactions = listOf(),
        file = null
      )

    fun getGroupEventSample() =
      ChatItem(
        chatDir = CIDirection.DirectRcv(),
        meta = CIMeta.getSample(1, Clock.System.now(), "group event text", CIStatus.RcvRead()),
        content = CIContent.RcvGroupEventContent(rcvGroupEvent = RcvGroupEvent.MemberAdded(groupMemberId = 1, profile = Profile.sampleData)),
        quotedItem = null,
        reactions = listOf(),
        file = null
      )

    fun getChatFeatureSample(feature: ChatFeature, enabled: FeatureEnabled): ChatItem {
      val content = CIContent.RcvChatFeature(feature = feature, enabled = enabled, param = null)
      return ChatItem(
        chatDir = CIDirection.DirectRcv(),
        meta = CIMeta.getSample(1, Clock.System.now(), content.text, CIStatus.RcvRead()),
        content = content,
        quotedItem = null,
        reactions = listOf(),
        file = null
      )
    }

    private const val TEMP_DELETED_CHAT_ITEM_ID = -1L
    const val TEMP_LIVE_CHAT_ITEM_ID = -2L

    val deletedItemDummy: ChatItem
      get() = ChatItem(
        chatDir = CIDirection.DirectRcv(),
        meta = CIMeta(
          itemId = TEMP_DELETED_CHAT_ITEM_ID,
          itemTs = Clock.System.now(),
          itemText = generalGetString(MR.strings.deleted_description),
          itemStatus = CIStatus.RcvRead(),
          sentViaProxy = null,
          createdAt = Clock.System.now(),
          updatedAt = Clock.System.now(),
          itemForwarded = null,
          itemDeleted = null,
          itemEdited = false,
          itemTimed = null,
          itemLive = false,
          deletable = false,
          editable = false
        ),
        content = CIContent.RcvDeleted(deleteMode = CIDeleteMode.cidmBroadcast),
        quotedItem = null,
        reactions = listOf(),
        file = null
      )

    fun liveDummy(direct: Boolean): ChatItem = ChatItem(
        chatDir = if (direct) CIDirection.DirectSnd() else CIDirection.GroupSnd(),
        meta = CIMeta(
          itemId = TEMP_LIVE_CHAT_ITEM_ID,
          itemTs = Clock.System.now(),
          itemText = "",
          itemStatus = CIStatus.RcvRead(),
          sentViaProxy = null,
          createdAt = Clock.System.now(),
          updatedAt = Clock.System.now(),
          itemForwarded = null,
          itemDeleted = null,
          itemEdited = false,
          itemTimed = null,
          itemLive = true,
          deletable = false,
          editable = false
        ),
        content = CIContent.SndMsgContent(MsgContent.MCText("")),
        quotedItem = null,
        reactions = listOf(),
        file = null
      )

    fun invalidJSON(chatDir: CIDirection?, meta: CIMeta?, json: String): ChatItem =
      ChatItem(
        chatDir = chatDir ?: CIDirection.DirectSnd(),
        meta = meta ?: CIMeta.invalidJSON(),
        content = CIContent.InvalidJSON(json),
        quotedItem = null,
        reactions = listOf(),
        file = null
      )
  }
}

fun MutableState<SnapshotStateList<Chat>>.add(index: Int, elem: Chat) {
  value = SnapshotStateList<Chat>().apply { addAll(value); add(index, elem) }
}

fun MutableState<SnapshotStateList<ChatItem>>.addAndNotify(index: Int, elem: ChatItem) {
  value = SnapshotStateList<ChatItem>().apply { addAll(value); add(index, elem); chatItemsChangesListener?.added(elem.id to elem.isRcvNew, index) }
}

fun MutableState<SnapshotStateList<Chat>>.add(elem: Chat) {
  value = SnapshotStateList<Chat>().apply { addAll(value); add(elem) }
}

// For some reason, Kotlin version crashes if the list is empty
fun <T> MutableList<T>.removeAll(predicate: (T) -> Boolean): Boolean = if (isEmpty()) false else remAll(predicate)

// Adds item to chatItems and notifies a listener about newly added item
fun MutableState<SnapshotStateList<ChatItem>>.addAndNotify(elem: ChatItem) {
  value = SnapshotStateList<ChatItem>().apply { addAll(value); add(elem); chatItemsChangesListener?.added(elem.id to elem.isRcvNew, lastIndex) }
}

fun <T> MutableState<SnapshotStateList<T>>.addAll(index: Int, elems: List<T>) {
  value = SnapshotStateList<T>().apply { addAll(value); addAll(index, elems) }
}

fun <T> MutableState<SnapshotStateList<T>>.addAll(elems: List<T>) {
  value = SnapshotStateList<T>().apply { addAll(value); addAll(elems) }
}

fun MutableState<SnapshotStateList<Chat>>.removeAll(block: (Chat) -> Boolean) {
  value = SnapshotStateList<Chat>().apply { addAll(value); removeAll(block) }
}

// Removes item(s) from chatItems and notifies a listener about removed item(s)
fun MutableState<SnapshotStateList<ChatItem>>.removeAllAndNotify(block: (ChatItem) -> Boolean) {
  val toRemove = ArrayList<Triple<Long, Int, Boolean>>()
  value = SnapshotStateList<ChatItem>().apply {
    addAll(value)
    var i = 0
    removeAll {
      val remove = block(it)
      if (remove) toRemove.add(Triple(it.id, i, it.isRcvNew))
      i++
      remove
    }
  }
  if (toRemove.isNotEmpty()) {
    chatItemsChangesListener?.removed(toRemove, value)
  }
}

fun MutableState<SnapshotStateList<Chat>>.removeAt(index: Int): Chat {
  val new = SnapshotStateList<Chat>()
  new.addAll(value)
  val res = new.removeAt(index)
  value = new
  return res
}

fun MutableState<SnapshotStateList<ChatItem>>.removeLastAndNotify() {
  val removed: Triple<Long, Int, Boolean>
  value = SnapshotStateList<ChatItem>().apply {
    addAll(value)
    val remIndex = lastIndex
    val rem = removeLast()
    removed = Triple(rem.id, remIndex, rem.isRcvNew)
  }
  chatItemsChangesListener?.removed(listOf(removed), value)
}

fun <T> MutableState<SnapshotStateList<T>>.replaceAll(elems: List<T>) {
  value = SnapshotStateList<T>().apply { addAll(elems) }
}

fun MutableState<SnapshotStateList<Chat>>.clear() {
  value = SnapshotStateList()
}

// Removes all chatItems and notifies a listener about it
fun MutableState<SnapshotStateList<ChatItem>>.clearAndNotify() {
  value = SnapshotStateList()
  chatItemsChangesListener?.cleared()
}

fun <T> State<SnapshotStateList<T>>.asReversed(): MutableList<T> = value.asReversed()

fun <T> State<SnapshotStateList<T>>.toList(): List<T> = value.toList()

operator fun <T> State<SnapshotStateList<T>>.get(i: Int): T = value[i]

operator fun <T> State<SnapshotStateList<T>>.set(index: Int, elem: T) { value[index] = elem }

val State<List<Any>>.size: Int get() = value.size

enum class CIMergeCategory {
  MemberConnected,
  RcvGroupEvent,
  SndGroupEvent,
  SndItemDeleted,
  RcvItemDeleted,
  ChatFeature,
}

@Serializable
sealed class CIDirection {
  @Serializable @SerialName("directSnd") class DirectSnd: CIDirection()
  @Serializable @SerialName("directRcv") class DirectRcv: CIDirection()
  @Serializable @SerialName("groupSnd") class GroupSnd: CIDirection()
  @Serializable @SerialName("groupRcv") class GroupRcv(val groupMember: GroupMember): CIDirection()
  @Serializable @SerialName("localSnd") class LocalSnd: CIDirection()
  @Serializable @SerialName("localRcv") class LocalRcv: CIDirection()

  val sent: Boolean get() = when(this) {
    is DirectSnd -> true
    is DirectRcv -> false
    is GroupSnd -> true
    is GroupRcv -> false
    is LocalSnd -> true
    is LocalRcv -> false
  }
}

@Serializable
data class CIMeta (
  val itemId: Long,
  val itemTs: Instant,
  val itemText: String,
  val itemStatus: CIStatus,
  val sentViaProxy: Boolean?,
  val createdAt: Instant,
  val updatedAt: Instant,
  val itemForwarded: CIForwardedFrom?,
  val itemDeleted: CIDeleted?,
  val itemEdited: Boolean,
  val itemTimed: CITimed?,
  val itemLive: Boolean?,
  val deletable: Boolean,
  val editable: Boolean
) {
  val timestampText: String get() = getTimestampText(itemTs, true)

  val recent: Boolean get() = updatedAt + 10.toDuration(DurationUnit.SECONDS) > Clock.System.now()
  val isLive: Boolean get() = itemLive == true
  val disappearing: Boolean get() = !isRcvNew && itemTimed?.deleteAt != null

  val isRcvNew: Boolean get() = itemStatus is CIStatus.RcvNew

  fun statusIcon(
    primaryColor: Color,
    metaColor: Color = CurrentColors.value.colors.secondary,
    paleMetaColor: Color = CurrentColors.value.colors.secondary
  ): Pair<ImageResource, Color>? =
    itemStatus.statusIcon(primaryColor, metaColor, paleMetaColor)

  companion object {
    fun getSample(
      id: Long, ts: Instant, text: String, status: CIStatus = CIStatus.SndNew(), sentViaProxy: Boolean? = null,
      itemForwarded: CIForwardedFrom? = null, itemDeleted: CIDeleted? = null, itemEdited: Boolean = false,
      itemTimed: CITimed? = null, itemLive: Boolean = false, deletable: Boolean = true, editable: Boolean = true
    ): CIMeta =
      CIMeta(
        itemId = id,
        itemTs = ts,
        itemText = text,
        itemStatus = status,
        sentViaProxy = sentViaProxy,
        createdAt = ts,
        updatedAt = ts,
        itemForwarded = itemForwarded,
        itemDeleted = itemDeleted,
        itemEdited = itemEdited,
        itemTimed = itemTimed,
        itemLive = itemLive,
        deletable = deletable,
        editable = editable
      )

    fun invalidJSON(): CIMeta =
      CIMeta(
        // itemId can not be the same for different items, otherwise ChatView will crash
        itemId = Random.nextLong(-1000000L, -1000L),
        itemTs = Clock.System.now(),
        itemText = "invalid JSON",
        itemStatus = CIStatus.SndNew(),
        sentViaProxy = null,
        createdAt = Clock.System.now(),
        updatedAt = Clock.System.now(),
        itemForwarded = null,
        itemDeleted = null,
        itemEdited = false,
        itemTimed = null,
        itemLive = false,
        deletable = false,
        editable = false
      )
  }
}

@Serializable
data class CITimed(
  val ttl: Int,
  val deleteAt: Instant?
)

fun getTimestampDateText(t: Instant): String {
  val tz = TimeZone.currentSystemDefault()
  val time = t.toLocalDateTime(tz).toJavaLocalDateTime()
  val weekday = time.format(DateTimeFormatter.ofPattern("EEE"))
  val dayMonthYear = time.format(DateTimeFormatter.ofPattern(
    if (Clock.System.now().toLocalDateTime(tz).year == time.year) "d MMM" else "d MMM yyyy")
  )

  return "$weekday, $dayMonthYear"
}

fun getTimestampText(t: Instant, shortFormat: Boolean = false): String {
  val tz = TimeZone.currentSystemDefault()
  val now: LocalDateTime = Clock.System.now().toLocalDateTime(tz)
  val time: LocalDateTime = t.toLocalDateTime(tz)
  val period = now.date.minus(time.date)
  val recent = now.date == time.date ||
      (period.years == 0 && period.months == 0 && period.days == 1 && now.hour < 12 && time.hour >= 18 )
  val dateFormatter =
    if (recent || shortFormat) {
      DateTimeFormatter.ofLocalizedTime(FormatStyle.SHORT)
    } else {
      val dayMonthFormat = when (Locale.getDefault().country) {
        "US" -> "M/dd"
        "DE" -> "dd.MM"
        "RU" -> "dd.MM"
        else -> "dd/MM"
      }
      val dayMonthYearFormat = when (Locale.getDefault().country) {
        "US" -> "M/dd/yy"
        "DE" -> "dd.MM.yy"
        "RU" -> "dd.MM.yy"
        else -> "dd/MM/yy"
      }
      DateTimeFormatter.ofPattern(
       if (now.year == time.year) dayMonthFormat else dayMonthYearFormat
      )
//      DateTimeFormatter.ofLocalizedDate(FormatStyle.SHORT)
    }
  return time.toJavaLocalDateTime().format(dateFormatter)
}

fun localTimestamp(t: Instant): String {
  val tz = TimeZone.currentSystemDefault()
  val ts: LocalDateTime = t.toLocalDateTime(tz)
  val dateFormatter = DateTimeFormatter.ofLocalizedDateTime(FormatStyle.MEDIUM)
  return ts.toJavaLocalDateTime().format(dateFormatter)
}

fun localDate(t: Instant): String {
  val tz = TimeZone.currentSystemDefault()
  val ts: LocalDateTime = t.toLocalDateTime(tz)
  val dateFormatter = DateTimeFormatter.ofLocalizedDate(FormatStyle.MEDIUM)
  return ts.toJavaLocalDateTime().format(dateFormatter)
}

@Serializable
sealed class CIStatus {
  @Serializable @SerialName("sndNew") class SndNew: CIStatus()
  @Serializable @SerialName("sndSent") class SndSent(val sndProgress: SndCIStatusProgress): CIStatus()
  @Serializable @SerialName("sndRcvd") class SndRcvd(val msgRcptStatus: MsgReceiptStatus, val sndProgress: SndCIStatusProgress): CIStatus()
  @Serializable @SerialName("sndErrorAuth") class SndErrorAuth: CIStatus()
  @Serializable @SerialName("sndError") class CISSndError(val agentError: SndError): CIStatus()
  @Serializable @SerialName("sndWarning") class SndWarning(val agentError: SndError): CIStatus()
  @Serializable @SerialName("rcvNew") class RcvNew: CIStatus()
  @Serializable @SerialName("rcvRead") class RcvRead: CIStatus()
  @Serializable @SerialName("invalid") class Invalid(val text: String): CIStatus()

  fun statusIcon(
    primaryColor: Color,
    metaColor: Color = CurrentColors.value.colors.secondary,
    paleMetaColor: Color = CurrentColors.value.colors.secondary
  ): Pair<ImageResource, Color>? =
    when (this) {
      is SndNew -> null
      is SndSent -> when (this.sndProgress) {
        SndCIStatusProgress.Complete -> MR.images.ic_check_filled to metaColor
        SndCIStatusProgress.Partial -> MR.images.ic_check_filled to paleMetaColor
      }
      is SndRcvd -> when(this.msgRcptStatus) {
        MsgReceiptStatus.Ok -> when (this.sndProgress) {
          SndCIStatusProgress.Complete -> MR.images.ic_double_check to metaColor
          SndCIStatusProgress.Partial -> MR.images.ic_double_check to paleMetaColor
        }
        MsgReceiptStatus.BadMsgHash -> MR.images.ic_double_check to Color.Red
      }
      is SndErrorAuth -> MR.images.ic_close to Color.Red
      is CISSndError -> MR.images.ic_close to Color.Red
      is SndWarning -> MR.images.ic_warning_filled to WarningOrange
      is RcvNew -> MR.images.ic_circle_filled to primaryColor
      is RcvRead -> null
      is CIStatus.Invalid -> MR.images.ic_question_mark to metaColor
    }

  val statusInto: Pair<String, String>? get() = when (this) {
    is SndNew -> null
    is SndSent -> null
    is SndRcvd -> null
    is SndErrorAuth -> generalGetString(MR.strings.message_delivery_error_title) to generalGetString(MR.strings.message_delivery_error_desc)
    is CISSndError -> generalGetString(MR.strings.message_delivery_error_title) to agentError.errorInfo
    is SndWarning -> generalGetString(MR.strings.message_delivery_warning_title) to agentError.errorInfo
    is RcvNew -> null
    is RcvRead -> null
    is Invalid -> "Invalid status" to this.text
  }
}

@Serializable
sealed class SndError {
  @Serializable @SerialName("auth") class Auth: SndError()
  @Serializable @SerialName("quota") class Quota: SndError()
  @Serializable @SerialName("expired") class Expired: SndError()
  @Serializable @SerialName("relay") class Relay(val srvError: SrvError): SndError()
  @Serializable @SerialName("proxy") class Proxy(val proxyServer: String, val srvError: SrvError): SndError()
  @Serializable @SerialName("proxyRelay") class ProxyRelay(val proxyServer: String, val srvError: SrvError): SndError()
  @Serializable @SerialName("other") class Other(val sndError: String): SndError()

  val errorInfo: String get() = when (this) {
    is SndError.Auth -> generalGetString(MR.strings.snd_error_auth)
    is SndError.Quota -> generalGetString(MR.strings.snd_error_quota)
    is SndError.Expired -> generalGetString(MR.strings.snd_error_expired)
    is SndError.Relay -> generalGetString(MR.strings.snd_error_relay).format(srvError.errorInfo)
    is SndError.Proxy -> generalGetString(MR.strings.snd_error_proxy).format(proxyServer, srvError.errorInfo)
    is SndError.ProxyRelay -> generalGetString(MR.strings.snd_error_proxy_relay).format(proxyServer, srvError.errorInfo)
    is SndError.Other -> generalGetString(MR.strings.ci_status_other_error).format(sndError)
  }
}

@Serializable
sealed class SrvError {
  @Serializable @SerialName("host") class Host: SrvError()
  @Serializable @SerialName("version") class Version: SrvError()
  @Serializable @SerialName("other") class Other(val srvError: String): SrvError()

  val errorInfo: String get() = when (this) {
    is SrvError.Host -> generalGetString(MR.strings.srv_error_host)
    is SrvError.Version -> generalGetString(MR.strings.srv_error_version)
    is SrvError.Other -> srvError
  }
}

@Serializable
enum class MsgReceiptStatus {
  @SerialName("ok") Ok,
  @SerialName("badMsgHash") BadMsgHash;
}

@Serializable
enum class SndCIStatusProgress {
  @SerialName("partial") Partial,
  @SerialName("complete") Complete;
}

@Serializable
sealed class GroupSndStatus {
  @Serializable @SerialName("new") class New: GroupSndStatus()
  @Serializable @SerialName("forwarded") class Forwarded: GroupSndStatus()
  @Serializable @SerialName("inactive") class Inactive: GroupSndStatus()
  @Serializable @SerialName("sent") class Sent: GroupSndStatus()
  @Serializable @SerialName("rcvd") class Rcvd(val msgRcptStatus: MsgReceiptStatus): GroupSndStatus()
  @Serializable @SerialName("error") class Error(val agentError: SndError): GroupSndStatus()
  @Serializable @SerialName("warning") class Warning(val agentError: SndError): GroupSndStatus()
  @Serializable @SerialName("invalid") class Invalid(val text: String): GroupSndStatus()

  fun statusIcon(
    primaryColor: Color,
    metaColor: Color = CurrentColors.value.colors.secondary,
    paleMetaColor: Color = CurrentColors.value.colors.secondary
  ): Pair<ImageResource, Color> =
    when (this) {
      is New -> MR.images.ic_more_horiz to metaColor
      is Forwarded -> MR.images.ic_chevron_right_2 to metaColor
      is Inactive -> MR.images.ic_person_off to metaColor
      is Sent -> MR.images.ic_check_filled to metaColor
      is Rcvd -> when(this.msgRcptStatus) {
        MsgReceiptStatus.Ok -> MR.images.ic_double_check to metaColor
        MsgReceiptStatus.BadMsgHash -> MR.images.ic_double_check to Color.Red
      }
      is Error -> MR.images.ic_close to Color.Red
      is Warning -> MR.images.ic_warning_filled to WarningOrange
      is Invalid -> MR.images.ic_question_mark to metaColor
    }

  val statusInto: Pair<String, String>? get() = when (this) {
    is New -> null
    is Forwarded -> generalGetString(MR.strings.message_forwarded_title) to generalGetString(MR.strings.message_forwarded_desc)
    is Inactive -> generalGetString(MR.strings.member_inactive_title) to generalGetString(MR.strings.member_inactive_desc)
    is Sent -> null
    is Rcvd -> null
    is Error -> generalGetString(MR.strings.message_delivery_error_title) to agentError.errorInfo
    is Warning -> generalGetString(MR.strings.message_delivery_warning_title) to agentError.errorInfo
    is Invalid -> "Invalid status" to this.text
  }
}

@Serializable
sealed class CIDeleted {
  @Serializable @SerialName("deleted") class Deleted(val deletedTs: Instant?): CIDeleted()
  @Serializable @SerialName("blocked") class Blocked(val deletedTs: Instant?): CIDeleted()
  @Serializable @SerialName("blockedByAdmin") class BlockedByAdmin(val deletedTs: Instant?): CIDeleted()
  @Serializable @SerialName("moderated") class Moderated(val deletedTs: Instant?, val byGroupMember: GroupMember): CIDeleted()
}

@Serializable
enum class MsgDirection {
  @SerialName("rcv") Rcv,
  @SerialName("snd") Snd;
}

@Serializable
sealed class CIForwardedFrom {
  @Serializable @SerialName("unknown") object Unknown: CIForwardedFrom()
  @Serializable @SerialName("contact") class Contact(override val chatName: String, val msgDir: MsgDirection, val contactId: Long? = null, val chatItemId: Long? = null): CIForwardedFrom()
  @Serializable @SerialName("group") class Group(override val chatName: String, val msgDir: MsgDirection, val groupId: Long? = null, val chatItemId: Long? = null): CIForwardedFrom()

  open val chatName: String
    get() = when (this) {
        Unknown -> ""
        is Contact -> chatName
        is Group -> chatName
      }

  fun text(chatType: ChatType): String =
    if (chatType == ChatType.Local) {
      if (chatName.isEmpty()) {
        generalGetString(MR.strings.saved_description)
      } else {
        generalGetString(MR.strings.saved_from_description).format(chatName)
      }
    } else {
      generalGetString(MR.strings.forwarded_description)
    }
}

@Serializable
enum class CIDeleteMode(val deleteMode: String) {
  @SerialName("internal") cidmInternal("internal"),
  @SerialName("internalMark") cidmInternalMark("internalMark"),
  @SerialName("broadcast") cidmBroadcast("broadcast");
}

interface ItemContent {
  val text: String
}

@Serializable
sealed class CIContent: ItemContent {
  abstract val msgContent: MsgContent?

  @Serializable @SerialName("sndMsgContent") class SndMsgContent(override val msgContent: MsgContent): CIContent()
  @Serializable @SerialName("rcvMsgContent") class RcvMsgContent(override val msgContent: MsgContent): CIContent()
  // legacy - since v4.3.0 itemDeleted field is used
  @Serializable @SerialName("sndDeleted") class SndDeleted(val deleteMode: CIDeleteMode): CIContent() { override val msgContent: MsgContent? get() = null }
  // legacy - since v4.3.0 itemDeleted field is used
  @Serializable @SerialName("rcvDeleted")  class RcvDeleted(val deleteMode: CIDeleteMode): CIContent() { override val msgContent: MsgContent? get() = null }
  @Serializable @SerialName("sndCall") class SndCall(val status: CICallStatus, val duration: Int): CIContent() { override val msgContent: MsgContent? get() = null }
  @Serializable @SerialName("rcvCall") class RcvCall(val status: CICallStatus, val duration: Int): CIContent() { override val msgContent: MsgContent? get() = null }
  @Serializable @SerialName("rcvIntegrityError") class RcvIntegrityError(val msgError: MsgErrorType): CIContent() { override val msgContent: MsgContent? get() = null }
  @Serializable @SerialName("rcvDecryptionError") class RcvDecryptionError(val msgDecryptError: MsgDecryptError, val msgCount: UInt): CIContent() { override val msgContent: MsgContent? get() = null }
  @Serializable @SerialName("rcvGroupInvitation") class RcvGroupInvitation(val groupInvitation: CIGroupInvitation, val memberRole: GroupMemberRole): CIContent() { override val msgContent: MsgContent? get() = null }
  @Serializable @SerialName("sndGroupInvitation") class SndGroupInvitation(val groupInvitation: CIGroupInvitation, val memberRole: GroupMemberRole): CIContent() { override val msgContent: MsgContent? get() = null }
  @Serializable @SerialName("rcvDirectEvent") class RcvDirectEventContent(val rcvDirectEvent: RcvDirectEvent): CIContent() { override val msgContent: MsgContent? get() = null }
  @Serializable @SerialName("rcvGroupEvent") class RcvGroupEventContent(val rcvGroupEvent: RcvGroupEvent): CIContent() { override val msgContent: MsgContent? get() = null }
  @Serializable @SerialName("sndGroupEvent") class SndGroupEventContent(val sndGroupEvent: SndGroupEvent): CIContent() { override val msgContent: MsgContent? get() = null }
  @Serializable @SerialName("rcvConnEvent") class RcvConnEventContent(val rcvConnEvent: RcvConnEvent): CIContent() { override val msgContent: MsgContent? get() = null }
  @Serializable @SerialName("sndConnEvent") class SndConnEventContent(val sndConnEvent: SndConnEvent): CIContent() { override val msgContent: MsgContent? get() = null }
  @Serializable @SerialName("rcvChatFeature") class RcvChatFeature(val feature: ChatFeature, val enabled: FeatureEnabled, val param: Int? = null): CIContent() { override val msgContent: MsgContent? get() = null }
  @Serializable @SerialName("sndChatFeature") class SndChatFeature(val feature: ChatFeature, val enabled: FeatureEnabled, val param: Int? = null): CIContent() { override val msgContent: MsgContent? get() = null }
  @Serializable @SerialName("rcvChatPreference") class RcvChatPreference(val feature: ChatFeature, val allowed: FeatureAllowed, val param: Int? = null): CIContent() { override val msgContent: MsgContent? get() = null }
  @Serializable @SerialName("sndChatPreference") class SndChatPreference(val feature: ChatFeature, val allowed: FeatureAllowed, val param: Int? = null): CIContent() { override val msgContent: MsgContent? get() = null }
  @Serializable @SerialName("rcvGroupFeature") class RcvGroupFeature(val groupFeature: GroupFeature, val preference: GroupPreference, val param: Int? = null, val memberRole_: GroupMemberRole?): CIContent() { override val msgContent: MsgContent? get() = null }
  @Serializable @SerialName("sndGroupFeature") class SndGroupFeature(val groupFeature: GroupFeature, val preference: GroupPreference, val param: Int? = null, val memberRole_: GroupMemberRole?): CIContent() { override val msgContent: MsgContent? get() = null }
  @Serializable @SerialName("rcvChatFeatureRejected") class RcvChatFeatureRejected(val feature: ChatFeature): CIContent() { override val msgContent: MsgContent? get() = null }
  @Serializable @SerialName("rcvGroupFeatureRejected") class RcvGroupFeatureRejected(val groupFeature: GroupFeature): CIContent() { override val msgContent: MsgContent? get() = null }
  @Serializable @SerialName("sndModerated") object SndModerated: CIContent() { override val msgContent: MsgContent? get() = null }
  @Serializable @SerialName("rcvModerated") object RcvModerated: CIContent() { override val msgContent: MsgContent? get() = null }
  @Serializable @SerialName("rcvBlocked") object RcvBlocked: CIContent() { override val msgContent: MsgContent? get() = null }
  @Serializable @SerialName("sndDirectE2EEInfo") class SndDirectE2EEInfo(val e2eeInfo: E2EEInfo): CIContent() { override val msgContent: MsgContent? get() = null }
  @Serializable @SerialName("rcvDirectE2EEInfo") class RcvDirectE2EEInfo(val e2eeInfo: E2EEInfo): CIContent() { override val msgContent: MsgContent? get() = null }
  @Serializable @SerialName("sndGroupE2EEInfo") class SndGroupE2EEInfo(val e2eeInfo: E2EEInfo): CIContent() { override val msgContent: MsgContent? get() = null }
  @Serializable @SerialName("rcvGroupE2EEInfo") class RcvGroupE2EEInfo(val e2eeInfo: E2EEInfo): CIContent() { override val msgContent: MsgContent? get() = null }
  @Serializable @SerialName("invalidJSON") data class InvalidJSON(val json: String): CIContent() { override val msgContent: MsgContent? get() = null }

  override val text: String get() = when (this) {
      is SndMsgContent -> msgContent.text
      is RcvMsgContent -> msgContent.text
      is SndDeleted -> generalGetString(MR.strings.deleted_description)
      is RcvDeleted -> generalGetString(MR.strings.deleted_description)
      is SndCall -> status.text(duration)
      is RcvCall -> status.text(duration)
      is RcvIntegrityError -> msgError.text
      is RcvDecryptionError -> msgDecryptError.text
      is RcvGroupInvitation -> groupInvitation.text
      is SndGroupInvitation -> groupInvitation.text
      is RcvDirectEventContent -> rcvDirectEvent.text
      is RcvGroupEventContent -> rcvGroupEvent.text
      is SndGroupEventContent -> sndGroupEvent.text
      is RcvConnEventContent -> rcvConnEvent.text
      is SndConnEventContent -> sndConnEvent.text
      is RcvChatFeature -> featureText(feature, enabled.text, param)
      is SndChatFeature -> featureText(feature, enabled.text, param)
      is RcvChatPreference -> preferenceText(feature, allowed, param)
      is SndChatPreference -> preferenceText(feature, allowed, param)
      is RcvGroupFeature -> featureText(groupFeature, preference.enable.text, param, memberRole_)
      is SndGroupFeature -> featureText(groupFeature, preference.enable.text, param, memberRole_)
      is RcvChatFeatureRejected -> "${feature.text}: ${generalGetString(MR.strings.feature_received_prohibited)}"
      is RcvGroupFeatureRejected -> "${groupFeature.text}: ${generalGetString(MR.strings.feature_received_prohibited)}"
      is SndModerated -> generalGetString(MR.strings.moderated_description)
      is RcvModerated -> generalGetString(MR.strings.moderated_description)
      is RcvBlocked -> generalGetString(MR.strings.blocked_by_admin_item_description)
      is SndDirectE2EEInfo -> directE2EEInfoStr(e2eeInfo)
      is RcvDirectE2EEInfo -> directE2EEInfoStr(e2eeInfo)
      is SndGroupE2EEInfo -> e2eeInfoNoPQStr
      is RcvGroupE2EEInfo -> e2eeInfoNoPQStr
      is InvalidJSON -> "invalid data"
    }

  val showMemberName: Boolean get() =
    when (this) {
      is RcvMsgContent -> true
      is RcvDeleted -> true
      is RcvCall -> true
      is RcvIntegrityError -> true
      is RcvDecryptionError -> true
      is RcvGroupInvitation -> true
      is RcvModerated -> true
      is RcvBlocked -> true
      is InvalidJSON -> true
      else -> false
    }

  companion object {
    fun directE2EEInfoStr(e2EEInfo: E2EEInfo): String =
      if (e2EEInfo.pqEnabled) {
        generalGetString(MR.strings.e2ee_info_pq_short)
      } else {
        e2eeInfoNoPQStr
      }

    private val e2eeInfoNoPQStr: String = generalGetString(MR.strings.e2ee_info_no_pq_short)

    fun featureText(feature: Feature, enabled: String, param: Int?, role: GroupMemberRole? = null): String =
      (if (feature.hasParam) {
        "${feature.text}: ${timeText(param)}"
      } else {
        "${feature.text}: $enabled"
      }) + (
          if (feature.hasRole && role != null)
            " (${roleText(role)})"
          else
            ""
          )

    private fun roleText(role: GroupMemberRole?): String =
      when (role) {
        GroupMemberRole.Owner -> generalGetString(MR.strings.feature_roles_owners)
        GroupMemberRole.Admin -> generalGetString(MR.strings.feature_roles_admins)
        else -> generalGetString(MR.strings.feature_roles_all_members)
      }

    fun preferenceText(feature: Feature, allowed: FeatureAllowed, param: Int?): String = when {
      allowed != FeatureAllowed.NO && feature.hasParam && param != null ->
        String.format(generalGetString(MR.strings.feature_offered_item_with_param), feature.text, timeText(param))
      allowed != FeatureAllowed.NO ->
        String.format(generalGetString(MR.strings.feature_offered_item), feature.text, timeText(param))
      else ->
        String.format(generalGetString(MR.strings.feature_cancelled_item), feature.text, timeText(param))
    }
  }
}

@Serializable
enum class MsgDecryptError {
  @SerialName("ratchetHeader") RatchetHeader,
  @SerialName("tooManySkipped") TooManySkipped,
  @SerialName("ratchetEarlier") RatchetEarlier,
  @SerialName("other") Other,
  @SerialName("ratchetSync") RatchetSync;

  val text: String get() = when (this) {
    RatchetHeader -> generalGetString(MR.strings.decryption_error)
    TooManySkipped -> generalGetString(MR.strings.decryption_error)
    RatchetEarlier -> generalGetString(MR.strings.decryption_error)
    Other -> generalGetString(MR.strings.decryption_error)
    RatchetSync -> generalGetString(MR.strings.encryption_renegotiation_error)
  }
}

@Serializable
class CIQuote (
  val chatDir: CIDirection? = null,
  val itemId: Long? = null,
  val sharedMsgId: String? = null,
  val sentAt: Instant,
  val content: MsgContent,
  val formattedText: List<FormattedText>? = null
): ItemContent {
  override val text: String by lazy {
    if (content.text == "" && content is MsgContent.MCVoice)
      durationText(content.duration)
    else
      content.text
  }


  fun sender(membership: GroupMember?): String? = when (chatDir) {
    is CIDirection.DirectSnd -> generalGetString(MR.strings.sender_you_pronoun)
    is CIDirection.DirectRcv -> null
    is CIDirection.GroupSnd -> membership?.displayName ?: generalGetString(MR.strings.sender_you_pronoun)
    is CIDirection.GroupRcv -> chatDir.groupMember.displayName
    is CIDirection.LocalSnd -> generalGetString(MR.strings.sender_you_pronoun)
    is CIDirection.LocalRcv -> null
    null -> null
  }

  companion object {
    fun getSample(itemId: Long?, sentAt: Instant, text: String, chatDir: CIDirection?): CIQuote =
      CIQuote(chatDir = chatDir, itemId = itemId, sentAt = sentAt, content = MsgContent.MCText(text))
  }
}

@Serializable
class CIReactionCount(val reaction: MsgReaction, val userReacted: Boolean, val totalReacted: Int)

@Serializable(with = MsgReactionSerializer::class)
sealed class MsgReaction {
  @Serializable(with = MsgReactionSerializer::class) class Emoji(val emoji: MREmojiChar): MsgReaction()
  @Serializable(with = MsgReactionSerializer::class) class Unknown(val type: String? = null, val json: JsonElement): MsgReaction()

  val text: String get() = when (this) {
    is Emoji -> when (emoji) {
      MREmojiChar.Heart -> "❤️"
      else -> emoji.value
    }
    is Unknown -> "?"
  }

  companion object {
    val values: List<MsgReaction> get() = MREmojiChar.values().map(::Emoji)
  }
}

object MsgReactionSerializer : KSerializer<MsgReaction> {
  override val descriptor: SerialDescriptor = buildSerialDescriptor("MsgReaction", PolymorphicKind.SEALED) {
    element("Emoji", buildClassSerialDescriptor("Emoji") {
      element<String>("emoji")
    })
    element("Unknown", buildClassSerialDescriptor("Unknown"))
  }

  override fun deserialize(decoder: Decoder): MsgReaction {
    require(decoder is JsonDecoder)
    val json = decoder.decodeJsonElement()
    return if (json is JsonObject && "type" in json) {
      when(val t = json["type"]?.jsonPrimitive?.content ?: "") {
        "emoji" -> {
          val msgReaction = try {
            val emoji = Json.decodeFromString<MREmojiChar>(json["emoji"].toString())
            MsgReaction.Emoji(emoji)
          } catch (e: Throwable) {
            MsgReaction.Unknown(t, json)
          }
          msgReaction
        }
        else -> MsgReaction.Unknown(t, json)
      }
    } else {
      MsgReaction.Unknown("", json)
    }
  }

  override fun serialize(encoder: Encoder, value: MsgReaction) {
    require(encoder is JsonEncoder)
    val json = when (value) {
      is MsgReaction.Emoji ->
        buildJsonObject {
          put("type", "emoji")
          put("emoji", json.encodeToJsonElement(value.emoji))
        }
      is MsgReaction.Unknown -> value.json
    }
    encoder.encodeJsonElement(json)
  }
}

@Serializable
enum class MREmojiChar(val value: String) {
  @SerialName("👍") ThumbsUp("👍"),
  @SerialName("👎") ThumbsDown("👎"),
  @SerialName("😀") Smile("😀"),
  @SerialName("😢") Sad("😢"),
  @SerialName("❤") Heart("❤"),
  @SerialName("🚀") Launch("🚀");
}

@Serializable
data class CIFile(
  val fileId: Long,
  val fileName: String,
  val fileSize: Long,
  val fileSource: CryptoFile? = null,
  val fileStatus: CIFileStatus,
  val fileProtocol: FileProtocol
) {
  val loaded: Boolean = when (fileStatus) {
    is CIFileStatus.SndStored -> true
    is CIFileStatus.SndTransfer -> true
    is CIFileStatus.SndComplete -> true
    is CIFileStatus.SndCancelled -> true
    is CIFileStatus.SndError -> true
    is CIFileStatus.SndWarning -> true
    is CIFileStatus.RcvInvitation -> false
    is CIFileStatus.RcvAccepted -> false
    is CIFileStatus.RcvTransfer -> false
    is CIFileStatus.RcvAborted -> false
    is CIFileStatus.RcvCancelled -> false
    is CIFileStatus.RcvComplete -> true
    is CIFileStatus.RcvError -> false
    is CIFileStatus.RcvWarning -> false
    is CIFileStatus.Invalid -> false
  }

  @Transient
  val cancelAction: CancelAction? = when (fileStatus) {
    is CIFileStatus.SndStored -> sndCancelAction
    is CIFileStatus.SndTransfer -> sndCancelAction
    is CIFileStatus.SndComplete ->
      if (fileProtocol == FileProtocol.XFTP) {
        revokeCancelAction
      } else {
        null
      }
    is CIFileStatus.SndCancelled -> null
    is CIFileStatus.SndError -> null
    is CIFileStatus.SndWarning -> sndCancelAction
    is CIFileStatus.RcvInvitation -> null
    is CIFileStatus.RcvAccepted -> rcvCancelAction
    is CIFileStatus.RcvTransfer -> rcvCancelAction
    is CIFileStatus.RcvAborted -> null
    is CIFileStatus.RcvCancelled -> null
    is CIFileStatus.RcvComplete -> null
    is CIFileStatus.RcvError -> null
    is CIFileStatus.RcvWarning -> rcvCancelAction
    is CIFileStatus.Invalid -> null
  }

  val showStatusIconInSmallView: Boolean = when (fileStatus) {
    is CIFileStatus.SndStored -> fileProtocol != FileProtocol.LOCAL
    is CIFileStatus.SndTransfer -> true
    is CIFileStatus.SndComplete -> false
    is CIFileStatus.SndCancelled -> true
    is CIFileStatus.SndError -> true
    is CIFileStatus.SndWarning -> true
    is CIFileStatus.RcvInvitation -> false
    is CIFileStatus.RcvAccepted -> true
    is CIFileStatus.RcvTransfer -> true
    is CIFileStatus.RcvAborted -> true
    is CIFileStatus.RcvCancelled -> true
    is CIFileStatus.RcvComplete -> false
    is CIFileStatus.RcvError -> true
    is CIFileStatus.RcvWarning -> true
    is CIFileStatus.Invalid -> true
  }

  /**
   * DO NOT CALL this function in compose scope, [LaunchedEffect], [DisposableEffect] and so on. Only with [withBGApi] or [runBlocking].
   * Otherwise, it will be canceled when moving to another screen/item/view, etc
   * */
  suspend fun loadRemoteFile(allowToShowAlert: Boolean): Boolean {
    val rh = chatModel.currentRemoteHost.value
    val user = chatModel.currentUser.value
    if (rh == null || user == null || fileSource == null || !loaded) return false
    if (getLoadedFilePath(this) != null) return true
    if (cachedRemoteFileRequests.contains(fileSource)) return false

    val rf = RemoteFile(
      userId = user.userId,
      fileId = fileId,
      sent = fileStatus.sent,
      fileSource = fileSource
    )
    cachedRemoteFileRequests[fileSource] = false
    val showAlert = fileSize > 5_000_000 && allowToShowAlert
    if (showAlert) {
      AlertManager.shared.showAlertMsgWithProgress(
        title = generalGetString(MR.strings.loading_remote_file_title),
        text = generalGetString(MR.strings.loading_remote_file_desc)
      )
    }
    val res = chatModel.controller.getRemoteFile(rh.remoteHostId, rf)
    cachedRemoteFileRequests[fileSource] = res
    if (showAlert) {
      AlertManager.shared.hideAlert()
    }
    return res
  }

  fun forwardingAllowed(): Boolean = when {
    chatModel.connectedToRemote() && cachedRemoteFileRequests[fileSource] != false && loaded -> true
    getLoadedFilePath(this) != null -> true
    else -> false
  }

  companion object {
    fun getSample(
      fileId: Long = 1,
      fileName: String = "test.txt",
      fileSize: Long = 100,
      filePath: String? = "test.txt",
      fileStatus: CIFileStatus = CIFileStatus.RcvComplete
    ): CIFile =
      CIFile(fileId = fileId, fileName = fileName, fileSize = fileSize, fileSource = if (filePath == null) null else CryptoFile.plain(filePath), fileStatus = fileStatus, fileProtocol = FileProtocol.XFTP)

    val cachedRemoteFileRequests = SnapshotStateMap<CryptoFile, Boolean>()
  }
}

@Serializable
data class CryptoFile(
  val filePath: String,
  val cryptoArgs: CryptoFileArgs?
) {

  val isAbsolutePath: Boolean
    get() = File(filePath).isAbsolute

  @Transient
  private var tmpFile: File? = null

  fun createTmpFileIfNeeded(): File {
    if (tmpFile == null) {
      val tmpFile = File(tmpDir, UUID.randomUUID().toString())
      tmpFile.deleteOnExit()
      ChatModel.filesToDelete.add(tmpFile)
      this.tmpFile = tmpFile
    }
    return tmpFile!!
  }

  fun deleteTmpFile() {
    tmpFile?.delete()
  }

  private fun decryptToTmpFile(): URI? {
    val absoluteFilePath = if (isAbsolutePath) filePath else getAppFilePath(filePath)
    val tmpFile = createTmpFileIfNeeded()
    try {
      decryptCryptoFile(absoluteFilePath, cryptoArgs ?: return null, tmpFile.absolutePath)
    } catch (e: Exception) {
      Log.e(TAG, "Unable to decrypt crypto file: " + e.stackTraceToString())
      AlertManager.shared.showAlertMsg(title = generalGetString(MR.strings.error), text = e.stackTraceToString())
      return null
    }
    return tmpFile.toURI()
  }

  fun decryptedGet(): URI? {
    val decrypted = decryptedUris[filePath]
    return if (decrypted != null && decrypted.toFile().exists()) {
      decrypted
    } else {
      null
    }
  }

  fun decryptedGetOrCreate(): URI? {
    val decrypted = decryptedGet() ?: decryptToTmpFile()
    if (decrypted != null) {
      decryptedUris[filePath] = decrypted
    }
    return decrypted
  }

  companion object {
    fun plain(f: String): CryptoFile = CryptoFile(f, null)

    fun desktopPlain(f: URI): CryptoFile = CryptoFile(f.toFile().absolutePath, null)

    private val decryptedUris = mutableMapOf<String, URI>()
  }
}

@Serializable
data class CryptoFileArgs(val fileKey: String, val fileNonce: String)

class CancelAction(
  val uiActionId: StringResource,
  val alert: AlertInfo
)

class AlertInfo(
  val titleId: StringResource,
  val messageId: StringResource,
  val confirmId: StringResource
)

private val sndCancelAction: CancelAction = CancelAction(
  uiActionId = MR.strings.stop_file__action,
  alert = AlertInfo(
    titleId = MR.strings.stop_snd_file__title,
    messageId = MR.strings.stop_snd_file__message,
    confirmId = MR.strings.stop_file__confirm
  )
)
private val revokeCancelAction: CancelAction = CancelAction(
  uiActionId = MR.strings.revoke_file__action,
  alert = AlertInfo(
    titleId = MR.strings.revoke_file__title,
    messageId = MR.strings.revoke_file__message,
    confirmId = MR.strings.revoke_file__confirm
  )
)
private val rcvCancelAction: CancelAction = CancelAction(
  uiActionId = MR.strings.stop_file__action,
  alert = AlertInfo(
    titleId = MR.strings.stop_rcv_file__title,
    messageId = MR.strings.stop_rcv_file__message,
    confirmId = MR.strings.stop_file__confirm
  )
)

@Serializable
enum class FileProtocol {
  @SerialName("smp") SMP,
  @SerialName("xftp") XFTP,
  @SerialName("local") LOCAL;
}

@Serializable
sealed class CIFileStatus {
  @Serializable @SerialName("sndStored") object SndStored: CIFileStatus()
  @Serializable @SerialName("sndTransfer") class SndTransfer(val sndProgress: Long, val sndTotal: Long): CIFileStatus()
  @Serializable @SerialName("sndComplete") object SndComplete: CIFileStatus()
  @Serializable @SerialName("sndCancelled") object SndCancelled: CIFileStatus()
  @Serializable @SerialName("sndError") class SndError(val sndFileError: FileError): CIFileStatus()
  @Serializable @SerialName("sndWarning") class SndWarning(val sndFileError: FileError): CIFileStatus()
  @Serializable @SerialName("rcvInvitation") object RcvInvitation: CIFileStatus()
  @Serializable @SerialName("rcvAccepted") object RcvAccepted: CIFileStatus()
  @Serializable @SerialName("rcvTransfer") class RcvTransfer(val rcvProgress: Long, val rcvTotal: Long): CIFileStatus()
  @Serializable @SerialName("rcvAborted") object RcvAborted: CIFileStatus()
  @Serializable @SerialName("rcvComplete") object RcvComplete: CIFileStatus()
  @Serializable @SerialName("rcvCancelled") object RcvCancelled: CIFileStatus()
  @Serializable @SerialName("rcvError") class RcvError(val rcvFileError: FileError): CIFileStatus()
  @Serializable @SerialName("rcvWarning") class RcvWarning(val rcvFileError: FileError): CIFileStatus()
  @Serializable @SerialName("invalid") class Invalid(val text: String): CIFileStatus()

  val sent: Boolean get() = when (this) {
    is SndStored -> true
    is SndTransfer -> true
    is SndComplete -> true
    is SndCancelled -> true
    is SndError -> true
    is SndWarning -> true
    is RcvInvitation -> false
    is RcvAccepted -> false
    is RcvTransfer -> false
    is RcvAborted -> false
    is RcvComplete -> false
    is RcvCancelled -> false
    is RcvError -> false
    is RcvWarning -> false
    is Invalid -> false
  }
}

@Serializable
sealed class FileError {
  @Serializable @SerialName("auth") class Auth: FileError()
  @Serializable @SerialName("noFile") class NoFile: FileError()
  @Serializable @SerialName("relay") class Relay(val srvError: SrvError): FileError()
  @Serializable @SerialName("other") class Other(val fileError: String): FileError()

  val errorInfo: String get() = when (this) {
    is FileError.Auth -> generalGetString(MR.strings.file_error_auth)
    is FileError.NoFile -> generalGetString(MR.strings.file_error_no_file)
    is FileError.Relay -> generalGetString(MR.strings.file_error_relay).format(srvError.errorInfo)
    is FileError.Other -> generalGetString(MR.strings.ci_status_other_error).format(fileError)
  }
}

@Suppress("SERIALIZER_TYPE_INCOMPATIBLE")
@Serializable(with = MsgContentSerializer::class)
sealed class MsgContent {
  abstract val text: String

  @Serializable(with = MsgContentSerializer::class) class MCText(override val text: String): MsgContent()
  @Serializable(with = MsgContentSerializer::class) class MCLink(override val text: String, val preview: LinkPreview): MsgContent()
  @Serializable(with = MsgContentSerializer::class) class MCImage(override val text: String, val image: String): MsgContent()
  @Serializable(with = MsgContentSerializer::class) class MCVideo(override val text: String, val image: String, val duration: Int): MsgContent()
  @Serializable(with = MsgContentSerializer::class) class MCVoice(override val text: String, val duration: Int): MsgContent()
  @Serializable(with = MsgContentSerializer::class) class MCFile(override val text: String): MsgContent()
  @Serializable(with = MsgContentSerializer::class) class MCReport(override val text: String, val reason: ReportReason): MsgContent()
  @Serializable(with = MsgContentSerializer::class) class MCUnknown(val type: String? = null, override val text: String, val json: JsonElement): MsgContent()

  val isVoice: Boolean get() =
    when (this) {
      is MCVoice -> true
      else -> false
    }

  val isMediaOrFileAttachment: Boolean get() =
    when (this) {
      is MCImage -> true
      is MCVideo -> true
      is MCFile -> true
      else -> false
    }

  val cmdString: String get() =
    if (this is MCUnknown) "json $json" else "json ${json.encodeToString(this)}"
}

@Serializable
class CIGroupInvitation (
  val groupId: Long,
  val groupMemberId: Long,
  val localDisplayName: String,
  val groupProfile: GroupProfile,
  val status: CIGroupInvitationStatus,
  ) {
  val text: String get() = String.format(
    generalGetString(MR.strings.group_invitation_item_description),
    groupProfile.displayName)

  companion object {
    fun getSample(
      groupId: Long = 1,
      groupMemberId: Long = 1,
      localDisplayName: String = "team",
      groupProfile: GroupProfile = GroupProfile.sampleData,
      status: CIGroupInvitationStatus = CIGroupInvitationStatus.Pending
    ): CIGroupInvitation =
      CIGroupInvitation(groupId = groupId, groupMemberId = groupMemberId, localDisplayName = localDisplayName, groupProfile = groupProfile, status = status)
  }
}

@Serializable
enum class CIGroupInvitationStatus {
  @SerialName("pending") Pending,
  @SerialName("accepted") Accepted,
  @SerialName("rejected") Rejected,
  @SerialName("expired") Expired;
}

@Serializable
class E2EEInfo (val pqEnabled: Boolean) {}

object MsgContentSerializer : KSerializer<MsgContent> {
  override val descriptor: SerialDescriptor = buildSerialDescriptor("MsgContent", PolymorphicKind.SEALED) {
    element("MCText", buildClassSerialDescriptor("MCText") {
      element<String>("text")
    })
    element("MCLink", buildClassSerialDescriptor("MCLink") {
      element<String>("text")
      element<String>("preview")
    })
    element("MCImage", buildClassSerialDescriptor("MCImage") {
      element<String>("text")
      element<String>("image")
    })
    element("MCVideo", buildClassSerialDescriptor("MCVideo") {
      element<String>("text")
      element<String>("image")
      element<Int>("duration")
    })
    element("MCFile", buildClassSerialDescriptor("MCFile") {
      element<String>("text")
    })
    element("MCReport", buildClassSerialDescriptor("MCReport") {
      element<String>("text")
      element<ReportReason>("reason")
    })
    element("MCUnknown", buildClassSerialDescriptor("MCUnknown"))
  }

  override fun deserialize(decoder: Decoder): MsgContent {
    require(decoder is JsonDecoder)
    val json = decoder.decodeJsonElement()
    return if (json is JsonObject) {
      if ("type" in json) {
        val t = json["type"]?.jsonPrimitive?.content ?: ""
        val text = json["text"]?.jsonPrimitive?.content ?: generalGetString(MR.strings.unknown_message_format)
        when (t) {
          "text" -> MsgContent.MCText(text)
          "link" -> {
            val preview = Json.decodeFromString<LinkPreview>(json["preview"].toString())
            MsgContent.MCLink(text, preview)
          }
          "image" -> {
            val image = json["image"]?.jsonPrimitive?.content ?: "unknown message format"
            MsgContent.MCImage(text, image)
          }
          "video" -> {
            val image = json["image"]?.jsonPrimitive?.content ?: "unknown message format"
            val duration = json["duration"]?.jsonPrimitive?.intOrNull ?: 0
            MsgContent.MCVideo(text, image, duration)
          }
          "voice" -> {
            val duration = json["duration"]?.jsonPrimitive?.intOrNull ?: 0
            MsgContent.MCVoice(text, duration)
          }
          "file" -> MsgContent.MCFile(text)
          "report" -> {
            val reason = Json.decodeFromString<ReportReason>(json["reason"].toString())
            MsgContent.MCReport(text, reason)
          }
          else -> MsgContent.MCUnknown(t, text, json)
        }
      } else {
        MsgContent.MCUnknown(text = generalGetString(MR.strings.invalid_message_format), json = json)
      }
    } else {
      MsgContent.MCUnknown(text = generalGetString(MR.strings.invalid_message_format), json = json)
    }
  }

  override fun serialize(encoder: Encoder, value: MsgContent) {
    require(encoder is JsonEncoder)
    val json = when (value) {
      is MsgContent.MCText ->
        buildJsonObject {
          put("type", "text")
          put("text", value.text)
        }
      is MsgContent.MCLink ->
        buildJsonObject {
          put("type", "link")
          put("text", value.text)
          put("preview", json.encodeToJsonElement(value.preview))
        }
      is MsgContent.MCImage ->
        buildJsonObject {
          put("type", "image")
          put("text", value.text)
          put("image", value.image)
        }
      is MsgContent.MCVideo ->
        buildJsonObject {
          put("type", "video")
          put("text", value.text)
          put("image", value.image)
          put("duration", value.duration)
        }
      is MsgContent.MCVoice ->
        buildJsonObject {
          put("type", "voice")
          put("text", value.text)
          put("duration", value.duration)
        }
      is MsgContent.MCFile ->
        buildJsonObject {
          put("type", "file")
          put("text", value.text)
        }
      is MsgContent.MCReport ->
        buildJsonObject {
          put("type", "report")
          put("text", value.text)
          put("reason", json.encodeToJsonElement(value.reason))
        }
      is MsgContent.MCUnknown -> value.json
    }
    encoder.encodeJsonElement(json)
  }
}

@Serializable
class FormattedText(val text: String, val format: Format? = null) {
  // TODO make it dependent on simplexLinkMode preference
  fun link(mode: SimplexLinkMode): String? = when (format) {
    is Format.Uri -> text
    is Format.SimplexLink -> if (mode == SimplexLinkMode.BROWSER) text else format.simplexUri
    is Format.Email -> "mailto:$text"
    is Format.Phone -> "tel:$text"
    else -> null
  }

  // TODO make it dependent on simplexLinkMode preference
  fun viewText(mode: SimplexLinkMode): String =
    if (format is Format.SimplexLink && mode == SimplexLinkMode.DESCRIPTION) simplexLinkText(format.linkType, format.smpHosts) else text

  fun simplexLinkText(linkType: SimplexLinkType, smpHosts: List<String>): String =
    "${linkType.description} (${String.format(generalGetString(MR.strings.simplex_link_connection), smpHosts.firstOrNull() ?: "?")})"
}

@Serializable
sealed class Format {
  @Serializable @SerialName("bold") class Bold: Format()
  @Serializable @SerialName("italic") class Italic: Format()
  @Serializable @SerialName("strikeThrough") class StrikeThrough: Format()
  @Serializable @SerialName("snippet") class Snippet: Format()
  @Serializable @SerialName("secret") class Secret: Format()
  @Serializable @SerialName("colored") class Colored(val color: FormatColor): Format()
  @Serializable @SerialName("uri") class Uri: Format()
  @Serializable @SerialName("simplexLink") class SimplexLink(val linkType: SimplexLinkType, val simplexUri: String, val smpHosts: List<String>): Format()
  @Serializable @SerialName("email") class Email: Format()
  @Serializable @SerialName("phone") class Phone: Format()

  val style: SpanStyle @Composable get() = when (this) {
    is Bold -> SpanStyle(fontWeight = FontWeight.Bold)
    is Italic -> SpanStyle(fontStyle = FontStyle.Italic)
    is StrikeThrough -> SpanStyle(textDecoration = TextDecoration.LineThrough)
    is Snippet -> SpanStyle(fontFamily = FontFamily.Monospace)
    is Secret -> SpanStyle(color = Color.Transparent, background = SecretColor)
    is Colored -> SpanStyle(color = this.color.uiColor)
    is Uri -> linkStyle
    is SimplexLink -> linkStyle
    is Email -> linkStyle
    is Phone -> linkStyle
  }

  val isSimplexLink = this is SimplexLink

  companion object {
    val linkStyle @Composable get() = SpanStyle(color = MaterialTheme.colors.primary, textDecoration = TextDecoration.Underline)
  }
}

@Serializable
enum class SimplexLinkType(val linkType: String) {
  contact("contact"),
  invitation("invitation"),
  group("group");

  val description: String get() = generalGetString(when (this) {
      contact -> MR.strings.simplex_link_contact
      invitation -> MR.strings.simplex_link_invitation
      group -> MR.strings.simplex_link_group
  })
}

@Serializable
enum class FormatColor(val color: String) {
  red("red"),
  green("green"),
  blue("blue"),
  yellow("yellow"),
  cyan("cyan"),
  magenta("magenta"),
  black("black"),
  white("white");

  val uiColor: Color @Composable get() = when (this) {
    red -> Color.Red
    green -> SimplexGreen
    blue -> SimplexBlue
    yellow -> WarningYellow
    cyan -> Color.Cyan
    magenta -> Color.Magenta
    black -> MaterialTheme.colors.onBackground
    white -> MaterialTheme.colors.onBackground
  }
}


@Serializable(with = ReportReasonSerializer::class)
sealed class ReportReason {
  @Serializable @SerialName("spam") object Spam: ReportReason()
  @Serializable @SerialName("illegal") object Illegal: ReportReason()
  @Serializable @SerialName("community") object Community: ReportReason()
  @Serializable @SerialName("profile") object Profile: ReportReason()
  @Serializable @SerialName("other") object Other: ReportReason()
  @Serializable @SerialName("unknown") data class Unknown(val type: String): ReportReason()

  companion object {
    val supportedReasons: List<ReportReason> = listOf(Spam, Illegal, Community, Profile, Other)
  }

  val text: String get() = when (this) {
    Spam -> generalGetString(MR.strings.report_reason_spam)
    Illegal -> generalGetString(MR.strings.report_reason_illegal)
    Community -> generalGetString(MR.strings.report_reason_community)
    Profile -> generalGetString(MR.strings.report_reason_profile)
    Other -> generalGetString(MR.strings.report_reason_other)
    is Unknown -> type
  }
}

object ReportReasonSerializer : KSerializer<ReportReason> {
  override val descriptor: SerialDescriptor =
    PrimitiveSerialDescriptor("ReportReason", PrimitiveKind.STRING)

  override fun deserialize(decoder: Decoder): ReportReason {
    return when (val value = decoder.decodeString()) {
      "spam" -> ReportReason.Spam
      "illegal" -> ReportReason.Illegal
      "community" -> ReportReason.Community
      "profile" -> ReportReason.Profile
      "other" -> ReportReason.Other
      else -> ReportReason.Unknown(value)
    }
  }

  override fun serialize(encoder: Encoder, value: ReportReason) {
    val stringValue = when (value) {
      is ReportReason.Spam -> "spam"
      is ReportReason.Illegal -> "illegal"
      is ReportReason.Community -> "community"
      is ReportReason.Profile -> "profile"
      is ReportReason.Other -> "other"
      is ReportReason.Unknown -> value.type
    }
    encoder.encodeString(stringValue)
  }
}

@Serializable
class SndFileTransfer() {}

@Serializable
data class RcvFileTransfer(
  val fileId: Long,
)

@Serializable
data class FileTransferMeta(
  val fileId: Long,
  val fileName: String,
  val filePath: String,
  val fileSize: Long,
)

@Serializable
enum class CICallStatus {
  @SerialName("pending") Pending,
  @SerialName("missed") Missed,
  @SerialName("rejected") Rejected,
  @SerialName("accepted") Accepted,
  @SerialName("negotiated") Negotiated,
  @SerialName("progress") Progress,
  @SerialName("ended") Ended,
  @SerialName("error") Error;

  fun text(sec: Int): String = when (this) {
    Pending -> generalGetString(MR.strings.callstatus_calling)
    Missed -> generalGetString(MR.strings.callstatus_missed)
    Rejected -> generalGetString(MR.strings.callstatus_rejected)
    Accepted -> generalGetString(MR.strings.callstatus_accepted)
    Negotiated -> generalGetString(MR.strings.callstatus_connecting)
    Progress -> generalGetString(MR.strings.callstatus_in_progress)
    Ended -> String.format(generalGetString(MR.strings.callstatus_ended), durationText(sec))
    Error -> generalGetString(MR.strings.callstatus_error)
  }
}

fun durationText(sec: Int): String {
  val s = sec % 60
  val m = sec / 60
  return if (m < 60) "%02d:%02d".format(m, s) else "%02d:%02d:%02d".format(m / 60, m % 60, s)
}

@Serializable
sealed class MsgErrorType() {
  @Serializable @SerialName("msgSkipped") class MsgSkipped(val fromMsgId: Long, val toMsgId: Long): MsgErrorType()
  @Serializable @SerialName("msgBadId") class MsgBadId(val msgId: Long): MsgErrorType()
  @Serializable @SerialName("msgBadHash") class MsgBadHash(): MsgErrorType()
  @Serializable @SerialName("msgDuplicate") class MsgDuplicate(): MsgErrorType()

  val text: String get() = when (this) {
    is MsgSkipped -> String.format(generalGetString(MR.strings.integrity_msg_skipped), toMsgId - fromMsgId + 1)
    is MsgBadHash -> generalGetString(MR.strings.integrity_msg_bad_hash) // not used now
    is MsgBadId -> generalGetString(MR.strings.integrity_msg_bad_id) // not used now
    is MsgDuplicate -> generalGetString(MR.strings.integrity_msg_duplicate) // not used now
  }
}

@Serializable
sealed class RcvDirectEvent() {
  @Serializable @SerialName("contactDeleted") class ContactDeleted(): RcvDirectEvent()
  @Serializable @SerialName("profileUpdated") class ProfileUpdated(val fromProfile: Profile, val toProfile: Profile): RcvDirectEvent()

  val text: String get() = when (this) {
    is ContactDeleted -> generalGetString(MR.strings.rcv_direct_event_contact_deleted)
    is ProfileUpdated -> profileUpdatedText(fromProfile, toProfile)
  }

  private fun profileUpdatedText(from: Profile, to: Profile): String =
    when {
      to.displayName != from.displayName || to.fullName != from.fullName ->
        generalGetString(MR.strings.profile_update_event_contact_name_changed).format(from.profileViewName, to.profileViewName)

      to.image != from.image -> when (to.image) {
        null -> generalGetString(MR.strings.profile_update_event_removed_picture)
        else -> generalGetString(MR.strings.profile_update_event_set_new_picture)
      }

      to.contactLink != from.contactLink -> when (to.contactLink) {
        null -> generalGetString(MR.strings.profile_update_event_removed_address)
        else -> generalGetString(MR.strings.profile_update_event_set_new_address)
      }
      // shouldn't happen if backend correctly creates item; UI should be synchronized with backend
      else -> generalGetString(MR.strings.profile_update_event_updated_profile)
    }
}

@Serializable
sealed class RcvGroupEvent() {
  @Serializable @SerialName("memberAdded") class MemberAdded(val groupMemberId: Long, val profile: Profile): RcvGroupEvent()
  @Serializable @SerialName("memberConnected") class MemberConnected(): RcvGroupEvent()
  @Serializable @SerialName("memberLeft") class MemberLeft(): RcvGroupEvent()
  @Serializable @SerialName("memberRole") class MemberRole(val groupMemberId: Long, val profile: Profile, val role: GroupMemberRole): RcvGroupEvent()
  @Serializable @SerialName("memberBlocked") class MemberBlocked(val groupMemberId: Long, val profile: Profile, val blocked: Boolean): RcvGroupEvent()
  @Serializable @SerialName("userRole") class UserRole(val role: GroupMemberRole): RcvGroupEvent()
  @Serializable @SerialName("memberDeleted") class MemberDeleted(val groupMemberId: Long, val profile: Profile): RcvGroupEvent()
  @Serializable @SerialName("userDeleted") class UserDeleted(): RcvGroupEvent()
  @Serializable @SerialName("groupDeleted") class GroupDeleted(): RcvGroupEvent()
  @Serializable @SerialName("groupUpdated") class GroupUpdated(val groupProfile: GroupProfile): RcvGroupEvent()
  @Serializable @SerialName("invitedViaGroupLink") class InvitedViaGroupLink(): RcvGroupEvent()
  @Serializable @SerialName("memberCreatedContact") class MemberCreatedContact(): RcvGroupEvent()
  @Serializable @SerialName("memberProfileUpdated") class MemberProfileUpdated(val fromProfile: Profile, val toProfile: Profile): RcvGroupEvent()

  val text: String get() = when (this) {
    is MemberAdded -> String.format(generalGetString(MR.strings.rcv_group_event_member_added), profile.profileViewName)
    is MemberConnected -> generalGetString(MR.strings.rcv_group_event_member_connected)
    is MemberLeft -> generalGetString(MR.strings.rcv_group_event_member_left)
    is MemberRole -> String.format(generalGetString(MR.strings.rcv_group_event_changed_member_role), profile.profileViewName, role.text)
    is MemberBlocked -> if (blocked) {
      String.format(generalGetString(MR.strings.rcv_group_event_member_blocked), profile.profileViewName)
    } else {
      String.format(generalGetString(MR.strings.rcv_group_event_member_unblocked), profile.profileViewName)
    }
    is UserRole -> String.format(generalGetString(MR.strings.rcv_group_event_changed_your_role), role.text)
    is MemberDeleted -> String.format(generalGetString(MR.strings.rcv_group_event_member_deleted), profile.profileViewName)
    is UserDeleted -> generalGetString(MR.strings.rcv_group_event_user_deleted)
    is GroupDeleted -> generalGetString(MR.strings.rcv_group_event_group_deleted)
    is GroupUpdated -> generalGetString(MR.strings.rcv_group_event_updated_group_profile)
    is InvitedViaGroupLink -> generalGetString(MR.strings.rcv_group_event_invited_via_your_group_link)
    is MemberCreatedContact -> generalGetString(MR.strings.rcv_group_event_member_created_contact)
    is MemberProfileUpdated -> profileUpdatedText(fromProfile, toProfile)
  }

  private fun profileUpdatedText(from: Profile, to: Profile): String =
    when {
      to.displayName != from.displayName || to.fullName != from.fullName ->
        generalGetString(MR.strings.profile_update_event_member_name_changed).format(from.profileViewName, to.profileViewName)

      to.image != from.image -> when (to.image) {
        null -> generalGetString(MR.strings.profile_update_event_removed_picture)
        else -> generalGetString(MR.strings.profile_update_event_set_new_picture)
      }
      // shouldn't happen if backend correctly creates item; UI should be synchronized with backend
      else -> generalGetString(MR.strings.profile_update_event_updated_profile)
    }
}

@Serializable
sealed class SndGroupEvent() {
  @Serializable @SerialName("memberRole") class MemberRole(val groupMemberId: Long, val profile: Profile, val role: GroupMemberRole): SndGroupEvent()
  @Serializable @SerialName("userRole") class UserRole(val role: GroupMemberRole): SndGroupEvent()
  @Serializable @SerialName("memberBlocked") class MemberBlocked(val groupMemberId: Long, val profile: Profile, val blocked: Boolean): SndGroupEvent()
  @Serializable @SerialName("memberDeleted") class MemberDeleted(val groupMemberId: Long, val profile: Profile): SndGroupEvent()
  @Serializable @SerialName("userLeft") class UserLeft(): SndGroupEvent()
  @Serializable @SerialName("groupUpdated") class GroupUpdated(val groupProfile: GroupProfile): SndGroupEvent()

  val text: String get() = when (this) {
    is MemberRole -> String.format(generalGetString(MR.strings.snd_group_event_changed_member_role), profile.profileViewName, role.text)
    is UserRole -> String.format(generalGetString(MR.strings.snd_group_event_changed_role_for_yourself), role.text)
    is MemberBlocked -> if (blocked) {
      String.format(generalGetString(MR.strings.snd_group_event_member_blocked), profile.profileViewName)
    } else {
      String.format(generalGetString(MR.strings.snd_group_event_member_unblocked), profile.profileViewName)
    }
    is MemberDeleted -> String.format(generalGetString(MR.strings.snd_group_event_member_deleted), profile.profileViewName)
    is UserLeft -> generalGetString(MR.strings.snd_group_event_user_left)
    is GroupUpdated -> generalGetString(MR.strings.snd_group_event_group_profile_updated)
  }
}

@Serializable
sealed class RcvConnEvent {
  @Serializable @SerialName("switchQueue") class SwitchQueue(val phase: SwitchPhase): RcvConnEvent()
  @Serializable @SerialName("ratchetSync") class RatchetSync(val syncStatus: RatchetSyncState): RcvConnEvent()
  @Serializable @SerialName("verificationCodeReset") object VerificationCodeReset: RcvConnEvent()
  @Serializable @SerialName("pqEnabled") class PQEnabled(val enabled: Boolean): RcvConnEvent()

  val text: String get() = when (this) {
    is SwitchQueue -> when (phase) {
      SwitchPhase.Completed -> generalGetString(MR.strings.rcv_conn_event_switch_queue_phase_completed)
      else -> generalGetString(MR.strings.rcv_conn_event_switch_queue_phase_changing)
    }
    is RatchetSync -> ratchetSyncStatusToText(syncStatus)
    is VerificationCodeReset -> generalGetString(MR.strings.rcv_conn_event_verification_code_reset)
    is PQEnabled -> if (enabled) {
      generalGetString(MR.strings.conn_event_enabled_pq)
    } else {
      generalGetString(MR.strings.conn_event_disabled_pq)
    }
  }
}

fun ratchetSyncStatusToText(ratchetSyncStatus: RatchetSyncState): String {
  return when (ratchetSyncStatus) {
    RatchetSyncState.Ok -> generalGetString(MR.strings.conn_event_ratchet_sync_ok)
    RatchetSyncState.Allowed -> generalGetString(MR.strings.conn_event_ratchet_sync_allowed)
    RatchetSyncState.Required -> generalGetString(MR.strings.conn_event_ratchet_sync_required)
    RatchetSyncState.Started -> generalGetString(MR.strings.conn_event_ratchet_sync_started)
    RatchetSyncState.Agreed -> generalGetString(MR.strings.conn_event_ratchet_sync_agreed)
  }
}

@Serializable
sealed class SndConnEvent {
  @Serializable @SerialName("switchQueue") class SwitchQueue(val phase: SwitchPhase, val member: GroupMemberRef? = null): SndConnEvent()
  @Serializable @SerialName("ratchetSync") class RatchetSync(val syncStatus: RatchetSyncState, val member: GroupMemberRef? = null): SndConnEvent()
  @Serializable @SerialName("pqEnabled") class PQEnabled(val enabled: Boolean): SndConnEvent()

  val text: String
    get() = when (this) {
      is SwitchQueue -> {
        member?.profile?.profileViewName?.let {
          return when (phase) {
            SwitchPhase.Completed -> String.format(generalGetString(MR.strings.snd_conn_event_switch_queue_phase_completed_for_member), it)
            else -> String.format(generalGetString(MR.strings.snd_conn_event_switch_queue_phase_changing_for_member), it)
          }
        }
        when (phase) {
          SwitchPhase.Completed -> generalGetString(MR.strings.snd_conn_event_switch_queue_phase_completed)
          else -> generalGetString(MR.strings.snd_conn_event_switch_queue_phase_changing)
        }
      }

      is RatchetSync -> {
        member?.profile?.profileViewName?.let {
          return when (syncStatus) {
            RatchetSyncState.Ok -> String.format(generalGetString(MR.strings.snd_conn_event_ratchet_sync_ok), it)
            RatchetSyncState.Allowed -> String.format(generalGetString(MR.strings.snd_conn_event_ratchet_sync_allowed), it)
            RatchetSyncState.Required -> String.format(generalGetString(MR.strings.snd_conn_event_ratchet_sync_required), it)
            RatchetSyncState.Started -> String.format(generalGetString(MR.strings.snd_conn_event_ratchet_sync_started), it)
            RatchetSyncState.Agreed -> String.format(generalGetString(MR.strings.snd_conn_event_ratchet_sync_agreed), it)
          }
        }
        ratchetSyncStatusToText(syncStatus)
      }

      is PQEnabled -> if (enabled) {
        generalGetString(MR.strings.conn_event_enabled_pq)
      } else {
        generalGetString(MR.strings.conn_event_disabled_pq)
      }
    }
}

@Serializable
enum class SwitchPhase {
  @SerialName("started") Started,
  @SerialName("confirmed") Confirmed,
  @SerialName("secured") Secured,
  @SerialName("completed") Completed
}

sealed class ChatItemTTL: Comparable<ChatItemTTL?> {
  object Day: ChatItemTTL()
  object Week: ChatItemTTL()
  object Month: ChatItemTTL()
  data class Seconds(val secs: Long): ChatItemTTL()
  object None: ChatItemTTL()

  override fun compareTo(other: ChatItemTTL?): Int = (seconds ?: Long.MAX_VALUE).compareTo(other?.seconds ?: Long.MAX_VALUE)

  val seconds: Long?
    get() =
      when (this) {
        is None -> null
        is Day -> 86400L
        is Week -> 7 * 86400L
        is Month -> 30 * 86400L
        is Seconds -> secs
      }

  companion object {
    fun fromSeconds(seconds: Long?): ChatItemTTL =
      when (seconds) {
        null -> None
        86400L -> Day
        7 * 86400L -> Week
        30 * 86400L -> Month
        else -> Seconds(seconds)
      }
  }
}

@Serializable
class ChatItemInfo(
  val itemVersions: List<ChatItemVersion>,
  val memberDeliveryStatuses: List<MemberDeliveryStatus>?,
  val forwardedFromChatItem: AChatItem?
)

@Serializable
data class ChatItemVersion(
  val chatItemVersionId: Long,
  val msgContent: MsgContent,
  val formattedText: List<FormattedText>?,
  val itemVersionTs: Instant,
  val createdAt: Instant,
)

@Serializable
data class MemberDeliveryStatus(
  val groupMemberId: Long,
  val memberDeliveryStatus: GroupSndStatus,
  val sentViaProxy: Boolean?
)

enum class NotificationPreviewMode {
  MESSAGE, CONTACT, HIDDEN;

  companion object {
    val default: NotificationPreviewMode = MESSAGE
  }
}

data class RemoteCtrlSession(
  val ctrlAppInfo: CtrlAppInfo?,
  val appVersion: String,
  val sessionState: UIRemoteCtrlSessionState
) {
  val active: Boolean
    get () = sessionState is UIRemoteCtrlSessionState.Connected

  val sessionCode: String?
    get() = when (val s = sessionState) {
      is UIRemoteCtrlSessionState.PendingConfirmation -> s.sessionCode
      is UIRemoteCtrlSessionState.Connected -> s.sessionCode
      else -> null
    }
}

@Serializable
sealed class RemoteCtrlSessionState {
  @Serializable @SerialName("starting") object Starting: RemoteCtrlSessionState()
  @Serializable @SerialName("searching") object Searching: RemoteCtrlSessionState()
  @Serializable @SerialName("connecting") object Connecting: RemoteCtrlSessionState()
  @Serializable @SerialName("pendingConfirmation") data class PendingConfirmation(val sessionCode: String): RemoteCtrlSessionState()
  @Serializable @SerialName("connected") data class Connected(val sessionCode: String): RemoteCtrlSessionState()
}

@Serializable
sealed class RemoteCtrlStopReason {
  @Serializable @SerialName("discoveryFailed") class DiscoveryFailed(val chatError: ChatError): RemoteCtrlStopReason()
  @Serializable @SerialName("connectionFailed") class ConnectionFailed(val chatError: ChatError): RemoteCtrlStopReason()
  @Serializable @SerialName("setupFailed") class SetupFailed(val chatError: ChatError): RemoteCtrlStopReason()
  @Serializable @SerialName("disconnected") object Disconnected: RemoteCtrlStopReason()
}

sealed class UIRemoteCtrlSessionState {
  object Starting: UIRemoteCtrlSessionState()
  object Searching: UIRemoteCtrlSessionState()
  data class Found(val remoteCtrl: RemoteCtrlInfo, val compatible: Boolean): UIRemoteCtrlSessionState()
  data class Connecting(val remoteCtrl_: RemoteCtrlInfo? = null): UIRemoteCtrlSessionState()
  data class PendingConfirmation(val remoteCtrl_: RemoteCtrlInfo? = null, val sessionCode: String): UIRemoteCtrlSessionState()
  data class Connected(val remoteCtrl: RemoteCtrlInfo, val sessionCode: String): UIRemoteCtrlSessionState()
}
