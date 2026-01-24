package chat.simplex.common.views.chat.group

import SectionDividerSpaced
import SectionItemView
import SectionItemViewLongClickable
import SectionSpacer
import SectionView
import androidx.compose.animation.*
import androidx.compose.animation.AnimatedVisibility
import androidx.compose.animation.core.animateDpAsState
import androidx.compose.foundation.Image
import androidx.compose.foundation.background
import androidx.compose.foundation.clickable
import androidx.compose.foundation.horizontalScroll
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.lazy.LazyListScope
import androidx.compose.foundation.lazy.items
import androidx.compose.foundation.rememberScrollState
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.runtime.saveable.rememberSaveable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.alpha
import androidx.compose.ui.draw.clip
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.graphics.ImageBitmap
import androidx.compose.ui.layout.ContentScale
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.input.TextFieldValue
import androidx.compose.ui.text.style.TextAlign
import androidx.compose.ui.text.style.TextOverflow
import androidx.compose.ui.unit.*
import chat.simplex.common.platform.base64ToBitmap
import kotlinx.datetime.*
import java.text.SimpleDateFormat
import java.util.*
import chat.simplex.common.model.*
import chat.simplex.common.model.ChatController.chatModel
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.chat.SelectedListItem
import chat.simplex.common.views.chat.SendReceipts
import chat.simplex.common.views.chat.item.ItemAction
import chat.simplex.common.views.chat.item.SelectItemAction
import chat.simplex.common.views.chat.group.showGroupReportsView
import chat.simplex.common.views.chatlist.cantInviteIncognitoAlert
import chat.simplex.common.views.chatlist.openChat
import chat.simplex.common.views.chatlist.UnreadBadge
import chat.simplex.common.views.chatlist.unreadCountStr
import chat.simplex.common.views.helpers.*
import chat.simplex.common.views.chat.item.*
import chat.simplex.common.views.usersettings.SettingsActionItem
import chat.simplex.common.views.usersettings.SettingsActionItemWithContent
import chat.simplex.common.platform.*
import chat.simplex.common.views.chat.ProviderMedia
import androidx.compose.ui.platform.LocalUriHandler
import java.io.File
import java.net.URI
import kotlinx.coroutines.runBlocking
import chat.simplex.res.MR
import dev.icerock.moko.resources.ImageResource
import dev.icerock.moko.resources.StringResource
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import kotlinx.coroutines.launch
import kotlin.math.sign

enum class GroupInfoTab {
  Members,
  Images,
  Videos,
  Files,
  Links,
  Voice
}

fun LazyListScope.GroupChatInfoTabs(
  groupInfo: GroupInfo,
  activeSortedMembers: List<GroupMember>,
  filteredMembers: State<List<GroupMember>>,
  searchText: MutableState<TextFieldValue>,
  selectedItems: MutableState<Set<Long>?>,
  scrollToItemId: MutableState<Long?>,
  addMembers: () -> Unit,
  showMemberInfo: (GroupMember) -> Unit,
  selectedTab: MutableState<GroupInfoTab>,
  filteredChatItems: State<List<ChatItem>>,
  chat: Chat,
  groupLink: GroupLink?,
  manageGroupLink: () -> Unit,
  openMemberSupport: () -> Unit,
  isLoadingTab: Boolean = false,
  onLoadMoreTabItems: (() -> Unit)? = null
) {

  item {
    TabRow(
      modifier = Modifier.padding(top = DEFAULT_PADDING_HALF, bottom = DEFAULT_PADDING),
      selectedTabIndex = GroupInfoTab.entries.indexOf(selectedTab.value),
      backgroundColor = Color.Transparent,
      contentColor = MaterialTheme.colors.primary,
    ) {
      GroupInfoTab.entries.forEachIndexed { index, tab ->
        val isSelected = selectedTab.value == tab
        LeadingIconTab(
          selected = isSelected,
          onClick = { selectedTab.value = tab },
          text = { Text("") },
          icon = {
            Column(
              modifier = Modifier.fillMaxWidth(),
              horizontalAlignment = Alignment.CenterHorizontally
            ) {
              Icon(
                painterResource(tabLabel(tab).first),
                contentDescription = tab.name,
                modifier = Modifier.size(24.dp),
                tint = if (isSelected) MaterialTheme.colors.primary else MaterialTheme.colors.secondary
              )
              // TODO make it conditional on the actual tab count
              if (GroupInfoTab.entries.size <= 4) {
                Text(generalGetString(tabLabel(tab).second), modifier = Modifier.padding(top = 2.dp, bottom = 6.dp), fontSize = 10.sp, softWrap = false, overflow = TextOverflow.Ellipsis)
              }
            }
          },
          selectedContentColor = MaterialTheme.colors.primary,
          unselectedContentColor = MaterialTheme.colors.secondary,
        )
      }
    }
  }

  when (selectedTab.value) {
    GroupInfoTab.Members -> {
      MembersTabContent(
        groupInfo = groupInfo,
        activeSortedMembers = activeSortedMembers,
        filteredMembers = filteredMembers,
        searchText = searchText,
        selectedItems = selectedItems,
        showMemberInfo = showMemberInfo,
        addMembers = addMembers,
        chat = chat,
        groupLink = groupLink,
        manageGroupLink = manageGroupLink,
        openMemberSupport = openMemberSupport,
        scrollToItemId = scrollToItemId
      )
    }
    GroupInfoTab.Images,
    GroupInfoTab.Videos,
    GroupInfoTab.Files,
    GroupInfoTab.Links,
    GroupInfoTab.Voice -> {
      ContentItemsTab(
        filteredChatItems = filteredChatItems,
        selectedTab = selectedTab.value,
        chat = chat,
        isLoading = isLoadingTab,
        onLoadMore = onLoadMoreTabItems
      )
    }
  }
}

private fun LazyListScope.MembersTabContent(
  groupInfo: GroupInfo,
  activeSortedMembers: List<GroupMember>,
  filteredMembers: State<List<GroupMember>>,
  searchText: MutableState<TextFieldValue>,
  selectedItems: MutableState<Set<Long>?>,
  showMemberInfo: (GroupMember) -> Unit,
  addMembers: () -> Unit,
  chat: Chat,
  groupLink: GroupLink?,
  manageGroupLink: () -> Unit,
  openMemberSupport: () -> Unit,
  scrollToItemId: MutableState<Long?>
) {

  item {
    val scope = rememberCoroutineScope()
    var anyTopSectionRowShow = false
    SectionView {
      if (groupInfo.canAddMembers && groupInfo.businessChat == null) {
        anyTopSectionRowShow = true
        if (groupLink == null) {
          CreateGroupLinkButton(manageGroupLink)
        } else {
          GroupLinkButton(manageGroupLink)
        }
      }
      if (groupInfo.businessChat == null && groupInfo.membership.memberRole >= GroupMemberRole.Moderator) {
        anyTopSectionRowShow = true
        MemberSupportButton(chat, openMemberSupport)
      }
      if (groupInfo.canModerate) {
        anyTopSectionRowShow = true
        GroupReportsButton(chat) {
          scope.launch {
            showGroupReportsView(chatModel.chatId, scrollToItemId, chat.chatInfo)
          }
        }
      }
      if (
        groupInfo.membership.memberActive &&
        (groupInfo.membership.memberRole < GroupMemberRole.Moderator || groupInfo.membership.supportChat != null)
      ) {
        anyTopSectionRowShow = true
        UserSupportChatButton(chat, groupInfo, scrollToItemId)
      }
    }
    if (anyTopSectionRowShow) {
      SectionDividerSpaced(maxBottomPadding = false)
    }
  }

  if (!groupInfo.nextConnectPrepared) {
    item {
      SectionView(title = String.format(generalGetString(MR.strings.group_info_section_title_num_members), activeSortedMembers.count() + 1)) {
        if (groupInfo.canAddMembers) {
          val onAddMembersClick = if (chat.chatInfo.incognito) ::cantInviteIncognitoAlert else addMembers
          val tint = if (chat.chatInfo.incognito) MaterialTheme.colors.secondary else MaterialTheme.colors.primary
          val addMembersTitleId = when (groupInfo.businessChat?.chatType) {
            BusinessChatType.Customer -> MR.strings.button_add_team_members
            BusinessChatType.Business -> MR.strings.button_add_friends
            null -> MR.strings.button_add_members
          }
          AddMembersButton(addMembersTitleId, tint, onAddMembersClick)
        }
        if (activeSortedMembers.size > 8) {
          SectionItemView(padding = PaddingValues(start = 14.dp, end = DEFAULT_PADDING_HALF)) {
            MemberListSearchRowView(searchText)
          }
        }
        SectionItemView(minHeight = 54.dp, padding = PaddingValues(horizontal = DEFAULT_PADDING)) {
          MemberRow(groupInfo.membership, user = true)
        }
      }
    }
  }
  if (!groupInfo.nextConnectPrepared) {
    items(filteredMembers.value, key = { it.groupMemberId }) { member ->
      Divider()
      val showMenu = remember { mutableStateOf(false) }
      val canBeSelected = groupInfo.membership.memberRole >= member.memberRole && member.memberRole < GroupMemberRole.Moderator
      SectionItemViewLongClickable(
        click = {
          if (selectedItems.value != null) {
            if (canBeSelected) {
              toggleItemSelection(member.groupMemberId, selectedItems)
            }
          } else {
            showMemberInfo(member)
          }
        },
        longClick = { showMenu.value = true },
        minHeight = 54.dp,
        padding = PaddingValues(horizontal = DEFAULT_PADDING)
      ) {
        Box(contentAlignment = Alignment.CenterStart) {
          this@SectionItemViewLongClickable.AnimatedVisibility(selectedItems.value != null, enter = fadeIn(), exit = fadeOut()) {
            SelectedListItem(Modifier.alpha(if (canBeSelected) 1f else 0f).padding(start = 2.dp), member.groupMemberId, selectedItems)
          }
          val selectionOffset by animateDpAsState(if (selectedItems.value != null) 20.dp + 22.dp * fontSizeMultiplier else 0.dp)
          DropDownMenuForMember(chat.remoteHostId, member, groupInfo, selectedItems, showMenu)
          Box(Modifier.padding(start = selectionOffset)) {
            MemberRow(member)
          }
        }
      }
    }
  }
}

private fun LazyListScope.ContentItemsTab(
  filteredChatItems: State<List<ChatItem>>,
  selectedTab: GroupInfoTab,
  chat: Chat,
  isLoading: Boolean = false,
  onLoadMore: (() -> Unit)? = null
) {
  if (isLoading && filteredChatItems.value.isEmpty()) {
    item {
      Box(
        Modifier
          .fillMaxWidth()
          .padding(vertical = DEFAULT_PADDING * 2),
        contentAlignment = Alignment.Center
      ) {
        ProgressIndicator()
      }
    }
  } else if (filteredChatItems.value.isEmpty()) {
    item {
      Box(
        Modifier
          .fillMaxWidth()
          .padding(vertical = DEFAULT_PADDING * 2),
        contentAlignment = Alignment.Center
      ) {
      //TODO: this is just a temporary UI, if no item, tab will not be shown
        Text(
          stringResource(MR.strings.no_items_found),
          color = MaterialTheme.colors.secondary
        )
      }
    }
  } else {
    when (selectedTab) {
      GroupInfoTab.Images, GroupInfoTab.Videos -> {
        MediaGridTabContent(
          chatItems = filteredChatItems.value,
          isVideo = selectedTab == GroupInfoTab.Videos
        )
      }
      GroupInfoTab.Files -> {
        MediaListTabContent(
          chatItems = filteredChatItems.value,
          type = MediaListType.Files,
          chat = chat
        )
      }
      GroupInfoTab.Links -> {
        MediaListTabContent(
          chatItems = filteredChatItems.value,
          type = MediaListType.Links,
          chat = chat
        )
      }
      GroupInfoTab.Voice -> {
        MediaListTabContent(
          chatItems = filteredChatItems.value,
          type = MediaListType.Voice,
          chat = chat
        )
      }
      else -> {}
    }
    
    // Load more button at the end
    if (onLoadMore != null && filteredChatItems.value.isNotEmpty()) {
      item {
        Box(
          Modifier
            .fillMaxWidth()
            .padding(vertical = DEFAULT_PADDING),
          contentAlignment = Alignment.Center
        ) {
          if (isLoading) {
            ProgressIndicator()
          } else {
            TextButton(
              onClick = { onLoadMore() },
              modifier = Modifier.padding(horizontal = DEFAULT_PADDING, vertical = DEFAULT_PADDING_HALF)
            ) {
              Text(
                stringResource(MR.strings.load_more),
                color = MaterialTheme.colors.primary
              )
            }
          }
        }
      }
    }
  }
}


@Composable
private fun tabLabel(tab: GroupInfoTab): Pair<ImageResource, StringResource> {
  return when (tab) {
    GroupInfoTab.Members -> MR.images.ic_group to MR.strings.group_info_tab_members
    GroupInfoTab.Images -> MR.images.ic_image to MR.strings.group_info_tab_images
    GroupInfoTab.Videos -> MR.images.ic_videocam to MR.strings.group_info_tab_videos
    GroupInfoTab.Voice -> MR.images.ic_mic_filled to MR.strings.group_info_tab_voice
    GroupInfoTab.Files -> MR.images.ic_draft to MR.strings.group_info_tab_files
    GroupInfoTab.Links -> MR.images.ic_link to MR.strings.group_info_tab_links
  }
}


// Helper enum for media list types
private enum class MediaListType {
  Files, Links, Voice
}

// Data class for date grouping
private data class DateGroup(
  val monthYear: String, // e.g., "December 2025"
  val items: List<ChatItem>
)

// Group items by month and year
private fun groupItemsByDate(items: List<ChatItem>): List<DateGroup> {
  val grouped = items.groupBy { item ->
    val tz = TimeZone.currentSystemDefault()
    val date = item.meta.itemTs.toLocalDateTime(tz).date
    val month = date.monthNumber
    val year = date.year
    
    // Format: "December 2025"
    val monthNames = arrayOf(
      "January", "February", "March", "April", "May", "June",
      "July", "August", "September", "October", "November", "December"
    )
    "${monthNames[month - 1]} $year"
  }
  
  // Sort by date (newest first)
  return grouped.map { (monthYear, items) ->
    DateGroup(monthYear, items.sortedByDescending { it.meta.itemTs })
  }.sortedByDescending { group ->
    val tz = TimeZone.currentSystemDefault()
    group.items.firstOrNull()?.meta?.itemTs?.toLocalDateTime(tz)?.date
  }
}

// Provider for gallery in group info tabs
private fun providerForGroupGallery(
  chatItems: List<ChatItem>,
  cItemId: Long
): ImageGalleryProvider {
  fun canShowMedia(item: ChatItem): Boolean =
    (item.content.msgContent is MsgContent.MCImage || item.content.msgContent is MsgContent.MCVideo) && 
    (item.file?.loaded == true && (getLoadedFilePath(item.file) != null || chatModel.connectedToRemote()))

  fun item(skipInternalIndex: Int, initialChatId: Long): Pair<Int, ChatItem>? {
    var processedInternalIndex = -skipInternalIndex.sign
    val indexOfFirst = chatItems.indexOfFirst { it.id == initialChatId }
    // The first was deleted or moderated
    if (indexOfFirst == -1) return null
    for (chatItemsIndex in if (skipInternalIndex >= 0) indexOfFirst downTo 0 else indexOfFirst..chatItems.lastIndex) {
      val item = chatItems[chatItemsIndex]
      if (canShowMedia(item)) {
        processedInternalIndex += skipInternalIndex.sign
      }
      if (processedInternalIndex == skipInternalIndex) {
        return chatItemsIndex to item
      }
    }
    return null
  }

  // Pager has a bug with overflowing when total pages is around Int.MAX_VALUE. Using smaller value
  var initialIndex = 10000 / 2
  var initialChatId = cItemId
  return object: ImageGalleryProvider {
    override val initialIndex: Int = initialIndex
    override val totalMediaSize = mutableStateOf(10000)
    override fun getMedia(index: Int): ProviderMedia? {
      val internalIndex = initialIndex - index
      val item = item(internalIndex, initialChatId)?.second ?: return null
      return when (item.content.msgContent) {
        is MsgContent.MCImage -> {
          val res = runBlocking { getLoadedImage(item.file) }
          val filePath = getLoadedFilePath(item.file)
          if (res != null && filePath != null) {
            val (imageBitmap: ImageBitmap, data: ByteArray) = res
            ProviderMedia.Image(data, imageBitmap)
          } else null
        }
        is MsgContent.MCVideo -> {
          val filePath = if (chatModel.connectedToRemote() && item.file?.loaded == true) getAppFilePath(item.file.fileName) else getLoadedFilePath(item.file)
          if (filePath != null) {
            val uri = getAppFileUri(filePath.substringAfterLast(File.separator))
            ProviderMedia.Video(uri, item.file?.fileSource, (item.content.msgContent as MsgContent.MCVideo).image)
          } else null
        }
        else -> null
      }
    }

    override fun currentPageChanged(index: Int) {
      val internalIndex = initialIndex - index
      val item = item(internalIndex, initialChatId) ?: return
      initialIndex = index
      initialChatId = item.second.id
    }

    override fun scrollToStart() {
      initialIndex = 0
      initialChatId = chatItems.firstOrNull { canShowMedia(it) }?.id ?: return
    }

    override fun onDismiss(index: Int) {
      // No-op for group info tabs
    }
  }
}

// Grid layout for images and videos
private fun LazyListScope.MediaGridTabContent(
  chatItems: List<ChatItem>,
  isVideo: Boolean
) {
  val groupedItems = groupItemsByDate(chatItems)
  
  groupedItems.forEach { group ->
    // Date header
    item {
      SectionView(title = group.monthYear) {}
    }
    
    // Grid items (4 columns)
    val columns = 4
    val rows = (group.items.size + columns - 1) / columns
    
    repeat(rows) { rowIndex ->
      item {
        Row(
          modifier = Modifier
            .fillMaxWidth()
            .padding(horizontal = DEFAULT_PADDING_HALF),
          horizontalArrangement = Arrangement.spacedBy(DEFAULT_PADDING_HALF)
        ) {
          repeat(columns) { colIndex ->
            val itemIndex = rowIndex * columns + colIndex
            val chatItem = if (itemIndex < group.items.size) group.items[itemIndex] else null
            Box(
              modifier = Modifier
                .weight(1f)
                .aspectRatio(1f)
                .padding(DEFAULT_PADDING_HALF)
                .clip(RoundedCornerShape(4.dp))
                .clickable { 
                  chatItem?.let { item ->
                    // Check if file is loaded before opening gallery
                    if (item.file?.loaded == true && (getLoadedFilePath(item.file) != null || chatModel.connectedToRemote())) {
                      val provider = providerForGroupGallery(chatItems, item.id)
                      ModalManager.fullscreen.showCustomModal(animated = false) { close ->
                        ImageFullScreenView({ provider }, close)
                      }
                    }
                    // If not loaded, do nothing
                  }
                }
            ) {
              if (chatItem != null) {
                MediaThumbnail(chatItem, isVideo)
              }
            }
          }
        }
      }
    }
  }
}

// Thumbnail for images/videos
@Composable
private fun MediaThumbnail(chatItem: ChatItem, isVideo: Boolean) {
  val content = chatItem.content.msgContent
  val image = when (content) {
    is MsgContent.MCImage -> content.image
    is MsgContent.MCVideo -> content.image
    else -> null
  }
  
  Box(modifier = Modifier.fillMaxSize()) {
    if (image != null) {
      Image(
        bitmap = base64ToBitmap(image),
        contentDescription = null,
        modifier = Modifier.fillMaxSize(),
        contentScale = ContentScale.Crop
      )
    } else {
      Box(
        modifier = Modifier
          .fillMaxSize()
          .background(MaterialTheme.colors.surface),
        contentAlignment = Alignment.Center
      ) {
        Icon(
          painterResource(if (isVideo) MR.images.ic_videocam else MR.images.ic_image),
          null,
          tint = MaterialTheme.colors.secondary
        )
      }
    }
    
    // Video play icon overlay
    if (isVideo || content is MsgContent.MCVideo) {
      Box(
        modifier = Modifier
          .fillMaxSize()
          .background(Color.Black.copy(alpha = 0.3f)),
        contentAlignment = Alignment.Center
      ) {
        Icon(
          painterResource(MR.images.ic_play_arrow_filled),
          null,
          modifier = Modifier.size(32.dp),
          tint = Color.White
        )
      }
    }
  }
}

// List layout for files, links, and voice
private fun LazyListScope.MediaListTabContent(
  chatItems: List<ChatItem>,
  type: MediaListType,
  chat: Chat
) {
  val groupedItems = groupItemsByDate(chatItems)
  
  groupedItems.forEach { group ->
    // Date header
    item {
      SectionView(title = group.monthYear) {}
    }
    
    // List items
    items(group.items, key = { it.id }) { chatItem ->
      Divider()
      when (type) {
        MediaListType.Files -> FileItemRow(chatItem, chat)
        MediaListType.Links -> LinkItemRow(chatItem)
        MediaListType.Voice -> VoiceItemRow(chatItem, chat)
      }
    }
  }
}

// File item row - reuses CIFileView component
@Composable
private fun FileItemRow(chatItem: ChatItem, chat: Chat) {
  val file = chatItem.file
  val senderName = getSenderName(chatItem)
  val dateTime = formatDateTimeForList(chatItem.meta.itemTs)
  val scope = rememberCoroutineScope()
  val user = chatModel.currentUser.value
  
  Column(
    Modifier.padding(horizontal = DEFAULT_PADDING, vertical = DEFAULT_PADDING_HALF)
  ) {
    // Sender name and date row
    Row(
      Modifier.fillMaxWidth(),
      horizontalArrangement = Arrangement.SpaceBetween,
      verticalAlignment = Alignment.CenterVertically
    ) {
      Text(
        senderName,
        maxLines = 1,
        overflow = TextOverflow.Ellipsis,
        style = MaterialTheme.typography.body2,
        fontWeight = FontWeight.Medium
      )
      Text(
        dateTime,
        fontSize = 11.sp,
        color = MaterialTheme.colors.secondary
      )
    }
    Spacer(Modifier.height(8.dp))
    // File component - using CIFileView which handles all file actions
    CIFileView(
      file = file,
      edited = chatItem.meta.itemEdited,
      showMenu = remember { mutableStateOf(false) },
      smallView = false,
      receiveFile = { fileId ->
        if (user != null) {
          scope.launch {
            withBGApi {
              chatModel.controller.receiveFile(chat.remoteHostId, user, fileId)
            }
          }
        }
      }
    )
  }
}

// Link item row
@Composable
private fun LinkItemRow(chatItem: ChatItem) {
  val content = chatItem.content.msgContent as? MsgContent.MCLink
    ?: return
  
  val preview = content.preview
  val senderName = getSenderName(chatItem)
  val dateTime = formatDateTimeForList(chatItem.meta.itemTs)
  val uriHandler = LocalUriHandler.current
  
  SectionItemView(
    click = { openBrowserAlert(preview.uri, uriHandler) },
    minHeight = 80.dp,
    padding = PaddingValues(horizontal = DEFAULT_PADDING, vertical = DEFAULT_PADDING_HALF)
  ) {
    Column(Modifier.fillMaxWidth()) {
      // Sender name and date row
      Row(
        Modifier.fillMaxWidth(),
        horizontalArrangement = Arrangement.SpaceBetween,
        verticalAlignment = Alignment.CenterVertically
      ) {
        Text(
          senderName,
          maxLines = 1,
          overflow = TextOverflow.Ellipsis,
          style = MaterialTheme.typography.body2,
          fontWeight = FontWeight.Medium
        )
        Text(
          dateTime,
          fontSize = 11.sp,
          color = MaterialTheme.colors.secondary
        )
      }
      Spacer(Modifier.height(8.dp))
      // Link preview - compact list style
      Row(
        Modifier.fillMaxWidth(),
        verticalAlignment = Alignment.CenterVertically
      ) {
        if (preview.image.isNotEmpty()) {
          Image(
            bitmap = base64ToBitmap(preview.image),
            contentDescription = stringResource(MR.strings.image_descr_link_preview),
            modifier = Modifier
              .size(64.dp)
              .clip(RoundedCornerShape(8.dp)),
            contentScale = ContentScale.Crop
          )
          Spacer(Modifier.width(DEFAULT_PADDING))
        } else {
          Icon(
            painterResource(MR.images.ic_link),
            null,
            modifier = Modifier.size(40.dp),
            tint = MaterialTheme.colors.primary
          )
          Spacer(Modifier.width(DEFAULT_PADDING))
        }
        Column(Modifier.weight(1f)) {
          if (preview.title.isNotEmpty()) {
            Text(
              preview.title,
              maxLines = 2,
              overflow = TextOverflow.Ellipsis,
              style = MaterialTheme.typography.body2
            )
            Spacer(Modifier.height(4.dp))
          }
          Text(
            preview.uri,
            maxLines = 1,
            overflow = TextOverflow.Ellipsis,
            fontSize = 12.sp,
            color = MaterialTheme.colors.secondary
          )
        }
      }
    }
  }
}

// Voice item row
@Composable
private fun VoiceItemRow(chatItem: ChatItem, chat: Chat) {
  val content = chatItem.content.msgContent as? MsgContent.MCVoice
  val duration = content?.duration ?: 0
  val senderName = getSenderName(chatItem)
  val dateTime = formatDateTimeForList(chatItem.meta.itemTs)
  val scope = rememberCoroutineScope()
  val user = chatModel.currentUser.value
  
  val receiveFile: (Long) -> Unit = { fileId ->
    if (user != null) {
      scope.launch {
        withBGApi {
          chatModel.controller.receiveFile(chat.remoteHostId, user, fileId)
        }
      }
    }
  }
  
  SectionItemView(
    click = { /* No action - voice playback is handled by CIVoiceView */ },
    minHeight = 64.dp,
    padding = PaddingValues(horizontal = DEFAULT_PADDING, vertical = DEFAULT_PADDING_HALF)
  ) {
    Column(Modifier.fillMaxWidth()) {
      // Sender name and date row
      Row(
        Modifier.fillMaxWidth(),
        horizontalArrangement = Arrangement.SpaceBetween,
        verticalAlignment = Alignment.CenterVertically
      ) {
        Text(
          senderName,
          maxLines = 1,
          overflow = TextOverflow.Ellipsis,
          style = MaterialTheme.typography.body2,
          fontWeight = FontWeight.Medium
        )
        Text(
          dateTime,
          fontSize = 11.sp,
          color = MaterialTheme.colors.secondary
        )
      }
      Spacer(Modifier.height(8.dp))

      CIVoiceView(
        providedDurationSec = duration,
        file = chatItem.file,
        edited = chatItem.meta.itemEdited,
        sent = chatItem.chatDir.sent,
        hasText = false,
        ci = chatItem,
        timedMessagesTTL = null,
        showViaProxy = false,
        showTimestamp = true,
        smallView = false,
        longClick = { },
        receiveFile = receiveFile
      )
    }
  }
}

// Get sender name from chat item
private fun getSenderName(chatItem: ChatItem): String {
  return when (val dir = chatItem.chatDir) {
    is CIDirection.GroupRcv -> dir.groupMember.chatViewName
    is CIDirection.GroupSnd -> "ME" // TODO: Replace with actual sender name
    else -> "Sender"
  }
}

// Format date/time for list items (e.g., "Dec 20 2022 12:01 PM")
private fun formatDateTimeForList(timestamp: Instant): String {
  val tz = TimeZone.currentSystemDefault()
  val dateTime = timestamp.toLocalDateTime(tz)
  val javaDate = dateTime.toJavaLocalDateTime()
  
  // Convert to milliseconds for SimpleDateFormat
  val millis = timestamp.epochSeconds * 1000L + timestamp.nanosecondsOfSecond / 1_000_000
  
  val dateFormat = SimpleDateFormat("MMM dd yyyy hh:mm a", Locale.getDefault())
  dateFormat.timeZone = java.util.TimeZone.getDefault()
  return dateFormat.format(Date(millis))
}

@Composable
private fun GroupChatItemRow(chatItem: ChatItem) {
  Row(
    Modifier.fillMaxWidth(),
    verticalAlignment = Alignment.CenterVertically
  ) {
    when (val content = chatItem.content.msgContent) {
      is MsgContent.MCImage -> {
        Icon(painterResource(MR.images.ic_image), null, Modifier.size(24.dp), tint = MaterialTheme.colors.secondary)
        Spacer(Modifier.width(DEFAULT_PADDING))
        Column(Modifier.weight(1f)) {
        //TODO, This is just a temporary UI, needs to be adjusted
          Text(
            content.text.ifEmpty { "Image" },
            maxLines = 1,
            overflow = TextOverflow.Ellipsis
          )
          Text(
            getTimestampText(chatItem.meta.itemTs),
            fontSize = 12.sp,
            color = MaterialTheme.colors.secondary
          )
        }
      }
      is MsgContent.MCVideo -> {
        Icon(painterResource(MR.images.ic_videocam), null, Modifier.size(24.dp), tint = MaterialTheme.colors.secondary)
        Spacer(Modifier.width(DEFAULT_PADDING))
        Column(Modifier.weight(1f)) {
          //TODO, This is just a temporary UI, needs to be adjusted
          Text(
            content.text.ifEmpty { "Video" },
            maxLines = 1,
            overflow = TextOverflow.Ellipsis
          )
          Text(
            getTimestampText(chatItem.meta.itemTs),
            fontSize = 12.sp,
            color = MaterialTheme.colors.secondary
          )
        }
      }
      is MsgContent.MCLink -> {
        Icon(painterResource(MR.images.ic_link), null, Modifier.size(24.dp), tint = MaterialTheme.colors.secondary)
        Spacer(Modifier.width(DEFAULT_PADDING))
        Column(Modifier.weight(1f)) {
          //TODO, This is just a temporary UI, needs to be adjusted
          Text(
            content.preview.uri.ifEmpty { content.text },
            maxLines = 1,
            overflow = TextOverflow.Ellipsis
          )
          Text(
            getTimestampText(chatItem.meta.itemTs),
            fontSize = 12.sp,
            color = MaterialTheme.colors.secondary
          )
        }
      }
      is MsgContent.MCFile -> {
        Icon(painterResource(MR.images.ic_draft_filled), null, Modifier.size(24.dp), tint = MaterialTheme.colors.secondary)
        Spacer(Modifier.width(DEFAULT_PADDING))
        Column(Modifier.weight(1f)) {
          //TODO, This is just a temporary UI, needs to be adjusted
          Text(
            chatItem.file?.fileName ?: content.text.ifEmpty { "File" },
            maxLines = 1,
            overflow = TextOverflow.Ellipsis
          )
          Text(
            getTimestampText(chatItem.meta.itemTs),
            fontSize = 12.sp,
            color = MaterialTheme.colors.secondary
          )
        }
      }
      is MsgContent.MCVoice -> {
        Icon(painterResource(MR.images.ic_mic_filled), null, Modifier.size(24.dp), tint = MaterialTheme.colors.secondary)
        Spacer(Modifier.width(DEFAULT_PADDING))
        Column(Modifier.weight(1f)) {
          //TODO, This is just a temporary UI, needs to be adjusted
          Text(
            content.text.ifEmpty { generalGetString(MR.strings.voice_message) },
            maxLines = 1,
            overflow = TextOverflow.Ellipsis
          )
          Text(
            getTimestampText(chatItem.meta.itemTs),
            fontSize = 12.sp,
            color = MaterialTheme.colors.secondary
          )
        }
      }
      else -> {
        Text(
          generalGetString(MR.strings.unknown_message_format),
          color = MaterialTheme.colors.secondary
        )
      }
    }
  }
}
