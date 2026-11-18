package chat.simplex.common.views.chat.group

import androidx.compose.foundation.background
import androidx.compose.foundation.clickable
import androidx.compose.foundation.interaction.MutableInteractionSource
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.lazy.LazyColumn
import androidx.compose.foundation.lazy.items
import androidx.compose.foundation.shape.CircleShape
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.clip
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.graphics.lerp
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.style.TextAlign
import androidx.compose.ui.text.style.TextOverflow
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import chat.simplex.common.model.*
import chat.simplex.common.platform.HapticPattern
import chat.simplex.common.platform.Log
import chat.simplex.common.platform.RichHaptics
import chat.simplex.common.platform.getAppFilePath
import chat.simplex.common.views.helpers.AlertManager
import kotlinx.serialization.encodeToString
import kotlinx.serialization.decodeFromString
import kotlinx.serialization.json.Json
import java.io.File
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.filled.Add
import androidx.compose.material.icons.filled.Close
import androidx.compose.material.icons.filled.Delete
import androidx.compose.material.icons.filled.Refresh
import androidx.compose.ui.graphics.vector.rememberVectorPainter

private const val MAX_FEEDS_PER_GROUP = 4

// Постоянное хранилище подписок в JSON-файле
object FeedStorage {
  private val json = Json { ignoreUnknownKeys = true; prettyPrint = true }
  private val feedsFile: File by lazy { File(getAppFilePath("zero_relay_feeds.json")) }
  private var cachedFeeds: MutableMap<Long, MutableList<FeedSubscription>>? = null

  private fun loadAll(): MutableMap<Long, MutableList<FeedSubscription>> {
    cachedFeeds?.let { return it }
    // Строго указываем возвращаемый тип для try-catch, чтобы избежать 'Any'
    val map: MutableMap<Long, MutableList<FeedSubscription>> = try {
      if (feedsFile.exists()) {
        val content = feedsFile.readText()
        // Явное указание типа переменной избавляет нас от угловых скобок в вызове функции
        val parsed: Map<Long, List<FeedSubscription>> = json.decodeFromString(content)
        
        val result = mutableMapOf<Long, MutableList<FeedSubscription>>()
        for ((key, value) in parsed) {
            result[key] = value.toMutableList()
        }
        result
      } else {
        // Явно указываем типы для пустого словаря
        mutableMapOf<Long, MutableList<FeedSubscription>>()
      }
    } catch (e: Exception) {
      Log.e("FeedStorage", "Error loading feeds: ${e.message}")
      mutableMapOf<Long, MutableList<FeedSubscription>>()
    }
    cachedFeeds = map
    return map
  }

  private fun saveAll() {
    try {
      val map = cachedFeeds ?: return
      // Явно сохраняем в базовый Map перед сериализацией
      val contentToSave: Map<Long, List<FeedSubscription>> = map
      val content = json.encodeToString(contentToSave)
      feedsFile.writeText(content)
    } catch (e: Exception) {
      Log.e("FeedStorage", "Error saving feeds: ${e.message}")
    }
  }

  @Synchronized
  fun getFeeds(groupId: Long): List<FeedSubscription> {
    return loadAll()[groupId]?.toList() ?: emptyList()
  }

  @Synchronized
  fun addFeed(groupId: Long, feed: FeedSubscription): Boolean {
    val map = loadAll()
    val list = map.getOrPut(groupId) { mutableListOf<FeedSubscription>() }
    if (list.size >= MAX_FEEDS_PER_GROUP) {
      return false
    }
    list.add(feed)
    saveAll()
    return true
  }

  @Synchronized
  fun updateFeed(groupId: Long, updated: FeedSubscription) {
    val map = loadAll()
    val list = map[groupId] ?: return
    val index = list.indexOfFirst { it.id == updated.id }
    if (index != -1) {
      list[index] = updated
      saveAll()
    }
  }

  @Synchronized
  fun removeFeed(groupId: Long, feedId: String) {
    val map = loadAll()
    val list = map[groupId] ?: return
    if (list.removeAll { it.id == feedId }) {
      saveAll()
    }
  }
}
// =====================================================================
// FULL-SCREEN MATERIAL 3 EXPRESSIVE VIEW
// =====================================================================
@Composable
fun GroupFeedsSheet(
  chatModel: ChatModel,
  groupInfo: GroupInfo,
  close: () -> Unit
) {
  var feeds by remember { mutableStateOf(FeedStorage.getFeeds(groupInfo.groupId)) }
  var showAddDialog by remember { mutableStateOf(false) }

  val isLight = MaterialTheme.colors.isLight
  val sheetBg = if (isLight) Color(0xFFF4F4F6) else Color(0xFF141416)
  val islandColor = lerp(MaterialTheme.colors.surface, MaterialTheme.colors.primary, if (isLight) 0.05f else 0.10f)
  val isLimitReached = feeds.size >= MAX_FEEDS_PER_GROUP

  // Полноэкранный фон от края до края
  Box(
    modifier = Modifier
      .fillMaxSize()
      .background(sheetBg)
      .statusBarsPadding()
      .navigationBarsPadding(),
    contentAlignment = Alignment.TopCenter
  ) {
    Column(
      modifier = Modifier
        .fillMaxSize()
        .padding(horizontal = 16.dp, vertical = 20.dp)
    ) {
      // 1. Верхний плавающий остров (Header)
      Surface(
        modifier = Modifier.fillMaxWidth(),
        shape = RoundedCornerShape(28.dp),
        color = islandColor,
        elevation = 2.dp
      ) {
        Row(
          modifier = Modifier
            .fillMaxWidth()
            .padding(horizontal = 20.dp, vertical = 18.dp),
          horizontalArrangement = Arrangement.SpaceBetween,
          verticalAlignment = Alignment.CenterVertically
        ) {
          Column(modifier = Modifier.weight(1f)) {
            Row(verticalAlignment = Alignment.CenterVertically) {
              Text(
                "Zero-Relay Feeds",
                style = MaterialTheme.typography.h6,
                fontWeight = FontWeight.ExtraBold,
                color = MaterialTheme.colors.onSurface
              )
              Spacer(Modifier.width(10.dp))
              Surface(
                shape = RoundedCornerShape(percent = 50),
                color = if (isLimitReached) MaterialTheme.colors.error.copy(alpha = 0.15f) else MaterialTheme.colors.primary.copy(alpha = 0.15f)
              ) {
                Text(
                  "${feeds.size}/$MAX_FEEDS_PER_GROUP",
                  modifier = Modifier.padding(horizontal = 10.dp, vertical = 4.dp),
                  fontSize = 12.sp,
                  fontWeight = FontWeight.Bold,
                  color = if (isLimitReached) MaterialTheme.colors.error else MaterialTheme.colors.primary
                )
              }
            }
            Spacer(Modifier.height(4.dp))
            Text(
              "RSS / Mastodon bridge to this chat",
              fontSize = 13.sp,
              color = MaterialTheme.colors.onSurface.copy(alpha = 0.65f)
            )
          }

          // Кнопка закрытия
          Surface(
            shape = CircleShape,
            color = MaterialTheme.colors.onSurface.copy(alpha = 0.08f),
            modifier = Modifier.size(44.dp)
          ) {
            IconButton(onClick = close) {
              Icon(
                painter = rememberVectorPainter(Icons.Default.Close),
                contentDescription = "Close",
                tint = MaterialTheme.colors.onSurface,
                modifier = Modifier.size(24.dp)
              )
            }
          }
        }
      }

      Spacer(Modifier.height(24.dp))

      // 2. Список Фидов
      if (feeds.isEmpty()) {
        Box(
          modifier = Modifier
            .fillMaxWidth()
            .weight(1f),
          contentAlignment = Alignment.Center
        ) {
          Column(horizontalAlignment = Alignment.CenterHorizontally) {
            Icon(
              painter = rememberVectorPainter(Icons.Default.Refresh),
              contentDescription = null,
              tint = MaterialTheme.colors.primary.copy(alpha = 0.5f),
              modifier = Modifier.size(72.dp)
            )
            Spacer(Modifier.height(16.dp))
            Text(
              "No active feeds yet.\nTap below to connect.",
              color = MaterialTheme.colors.secondary,
              fontSize = 16.sp,
              textAlign = TextAlign.Center
            )
          }
        }
      } else {
        LazyColumn(
          verticalArrangement = Arrangement.spacedBy(16.dp),
          contentPadding = PaddingValues(bottom = 100.dp), // Отступ для плавающей кнопки
          modifier = Modifier
            .fillMaxWidth()
            .weight(1f)
        ) {
          items(feeds, key = { it.id }) { sub ->
            FeedItemCard(
              sub = sub,
              cardBg = islandColor,
              onToggle = { enabled ->
                val updated = sub.copy(enabled = enabled)
                FeedStorage.updateFeed(groupInfo.groupId, updated)
                feeds = FeedStorage.getFeeds(groupInfo.groupId)
              },
              onSyncNow = {
                RichHaptics.perform(HapticPattern.TICK)
                ZeroRelayFeedEngine.syncFeed(chatModel, sub) { updated ->
                  FeedStorage.updateFeed(groupInfo.groupId, updated)
                  feeds = FeedStorage.getFeeds(groupInfo.groupId)
                }
              },
              onDelete = {
                RichHaptics.perform(HapticPattern.WARNING)
                FeedStorage.removeFeed(groupInfo.groupId, sub.id)
                feeds = FeedStorage.getFeeds(groupInfo.groupId)
              }
            )
          }
        }
      }
    }

    // 3. Плавающая кнопка добавления (FAB в стиле M3)
    Box(
      modifier = Modifier
        .align(Alignment.BottomCenter)
        .padding(bottom = 32.dp)
    ) {
      Surface(
        shape = RoundedCornerShape(24.dp),
        color = if (isLimitReached) MaterialTheme.colors.onSurface.copy(alpha = 0.12f) else MaterialTheme.colors.primary,
        elevation = if (isLimitReached) 0.dp else 8.dp,
        modifier = Modifier.height(64.dp)
      ) {
        Row(
          modifier = Modifier
            .clickable(enabled = !isLimitReached) {
              if (isLimitReached) {
                RichHaptics.perform(HapticPattern.WARNING)
                AlertManager.shared.showAlertMsg(
                  "Лимит исчерпан",
                  "В одном чате или канале можно подключить максимум $MAX_FEEDS_PER_GROUP источника."
                )
              } else {
                RichHaptics.perform(HapticPattern.CLICK)
                showAddDialog = true
              }
            }
            .padding(horizontal = 24.dp),
          verticalAlignment = Alignment.CenterVertically,
          horizontalArrangement = Arrangement.Center
        ) {
          Icon(
            painter = rememberVectorPainter(Icons.Default.Add),
            contentDescription = "Add feed",
            tint = if (isLimitReached) MaterialTheme.colors.onSurface.copy(alpha = 0.38f) else MaterialTheme.colors.onPrimary,
            modifier = Modifier.size(24.dp)
          )
          Spacer(Modifier.width(12.dp))
          Text(
            text = "New Feed",
            color = if (isLimitReached) MaterialTheme.colors.onSurface.copy(alpha = 0.38f) else MaterialTheme.colors.onPrimary,
            fontSize = 16.sp,
            fontWeight = FontWeight.Bold
          )
        }
      }
    }
  }

  if (showAddDialog) {
    AddFeedDialog(
      onDismiss = { showAddDialog = false },
      onAdd = { url, title, type ->
        val newSub = FeedSubscription(
          id = System.currentTimeMillis().toString(),
          targetGroupId = groupInfo.groupId,
          url = url,
          type = type,
          title = if (title.isNotBlank()) title else url.removePrefix("https://").removePrefix("http://").take(30),
          enabled = true,
          lastFetchedTs = 0L,
          seenItemIds = emptySet()
        )
        val added = FeedStorage.addFeed(groupInfo.groupId, newSub)
        if (added) {
          feeds = FeedStorage.getFeeds(groupInfo.groupId)
          showAddDialog = false
          ZeroRelayFeedEngine.syncFeed(chatModel, newSub) { updated ->
            FeedStorage.updateFeed(groupInfo.groupId, updated)
            feeds = FeedStorage.getFeeds(groupInfo.groupId)
          }
        }
      }
    )
  }
}

// =====================================================================
// M3 EXPRESSIVE FEED ITEM CARD
// =====================================================================
@Composable
private fun FeedItemCard(
  sub: FeedSubscription,
  cardBg: Color,
  onToggle: (Boolean) -> Unit,
  onSyncNow: () -> Unit,
  onDelete: () -> Unit
) {
  Surface(
    shape = RoundedCornerShape(28.dp),
    color = cardBg,
    elevation = 2.dp,
    modifier = Modifier.fillMaxWidth()
  ) {
    Column(
      modifier = Modifier
        .fillMaxWidth()
        .padding(20.dp)
    ) {
      Row(
        modifier = Modifier.fillMaxWidth(),
        verticalAlignment = Alignment.CenterVertically
      ) {
        // Иконка в мягком кружке
        Surface(
          shape = CircleShape,
          color = MaterialTheme.colors.primary.copy(alpha = 0.15f),
          modifier = Modifier.size(52.dp)
        ) {
          Icon(
            painter = rememberVectorPainter(if (sub.type == FeedType.RSS_ATOM) Icons.Default.Refresh else Icons.Default.Add),
            contentDescription = null,
            tint = MaterialTheme.colors.primary,
            modifier = Modifier
              .padding(14.dp)
              .fillMaxSize()
          )
        }

        Spacer(Modifier.width(16.dp))

        Column(modifier = Modifier.weight(1f)) {
          Text(
            text = sub.title,
            fontWeight = FontWeight.ExtraBold,
            fontSize = 17.sp,
            color = MaterialTheme.colors.onSurface,
            maxLines = 1,
            overflow = TextOverflow.Ellipsis
          )
          Spacer(Modifier.height(4.dp))
          Text(
            text = sub.url,
            fontSize = 13.sp,
            color = MaterialTheme.colors.onSurface.copy(alpha = 0.6f),
            maxLines = 1,
            overflow = TextOverflow.Ellipsis
          )
        }

        Spacer(Modifier.width(12.dp))

        Switch(
          checked = sub.enabled,
          onCheckedChange = {
            RichHaptics.perform(if (it) HapticPattern.TOGGLE_ON else HapticPattern.TOGGLE_OFF)
            onToggle(it)
          },
          colors = SwitchDefaults.colors(
            checkedThumbColor = MaterialTheme.colors.primary,
            checkedTrackColor = MaterialTheme.colors.primary.copy(alpha = 0.5f)
          )
        )
      }

      Spacer(Modifier.height(18.dp))
      Divider(color = MaterialTheme.colors.onSurface.copy(alpha = 0.08f))
      Spacer(Modifier.height(14.dp))

      Row(
        modifier = Modifier.fillMaxWidth(),
        horizontalArrangement = Arrangement.End,
        verticalAlignment = Alignment.CenterVertically
      ) {
        // Остров "Синхронизировать"
        Surface(
          shape = CircleShape,
          color = MaterialTheme.colors.primary.copy(alpha = 0.1f),
          modifier = Modifier.height(40.dp)
        ) {
          Row(
            modifier = Modifier
              .clickable { onSyncNow() }
              .padding(horizontal = 18.dp),
            verticalAlignment = Alignment.CenterVertically
          ) {
            Icon(
              painter = rememberVectorPainter(Icons.Default.Refresh),
              contentDescription = "Sync",
              tint = MaterialTheme.colors.primary,
              modifier = Modifier.size(18.dp)
            )
            Spacer(Modifier.width(8.dp))
            Text("Sync Now", color = MaterialTheme.colors.primary, fontSize = 14.sp, fontWeight = FontWeight.Bold)
          }
        }

        Spacer(Modifier.width(12.dp))

        // Остров "Удалить"
        Surface(
          shape = CircleShape,
          color = MaterialTheme.colors.error.copy(alpha = 0.1f),
          modifier = Modifier.height(40.dp)
        ) {
          Row(
            modifier = Modifier
              .clickable { onDelete() }
              .padding(horizontal = 18.dp),
            verticalAlignment = Alignment.CenterVertically
          ) {
            Icon(
              painter = rememberVectorPainter(Icons.Default.Delete),
              contentDescription = "Delete",
              tint = MaterialTheme.colors.error,
              modifier = Modifier.size(18.dp)
            )
            Spacer(Modifier.width(8.dp))
            Text("Delete", color = MaterialTheme.colors.error, fontSize = 14.sp, fontWeight = FontWeight.Bold)
          }
        }
      }
    }
  }
}

@Composable
private fun AddFeedDialog(
  onDismiss: () -> Unit,
  onAdd: (url: String, title: String, type: FeedType) -> Unit
) {
  var url by remember { mutableStateOf("") }
  var title by remember { mutableStateOf("") }
  var feedType by remember { mutableStateOf(FeedType.RSS_ATOM) }

  val isLight = MaterialTheme.colors.isLight
  val dialogBg = lerp(MaterialTheme.colors.surface, MaterialTheme.colors.primary, if (isLight) 0.08f else 0.12f)

  AlertDialog(
    onDismissRequest = onDismiss,
    shape = RoundedCornerShape(28.dp),
    backgroundColor = dialogBg,
    title = {
      Text(
        text = "Add Feed (Max $MAX_FEEDS_PER_GROUP)",
        style = MaterialTheme.typography.h6,
        fontWeight = FontWeight.Bold,
        color = MaterialTheme.colors.onSurface
      )
    },
    text = {
      Column(verticalArrangement = Arrangement.spacedBy(12.dp)) {
        Row(
          modifier = Modifier.fillMaxWidth(),
          horizontalArrangement = Arrangement.spacedBy(8.dp)
        ) {
          val isRss = feedType == FeedType.RSS_ATOM
          Button(
            onClick = {
              RichHaptics.perform(HapticPattern.TICK)
              feedType = FeedType.RSS_ATOM
            },
            modifier = Modifier.weight(1f).height(38.dp),
            shape = RoundedCornerShape(percent = 50),
            colors = ButtonDefaults.buttonColors(
              backgroundColor = if (isRss) MaterialTheme.colors.primary else MaterialTheme.colors.surface.copy(alpha = 0.5f),
              contentColor = if (isRss) MaterialTheme.colors.onPrimary else MaterialTheme.colors.onSurface
            ),
            elevation = null
          ) {
            Text("RSS / Atom", fontSize = 12.sp, fontWeight = FontWeight.Medium)
          }

          Button(
            onClick = {
              RichHaptics.perform(HapticPattern.TICK)
              feedType = FeedType.MASTODON_ACTIVITYPUB
            },
            modifier = Modifier.weight(1f).height(38.dp),
            shape = RoundedCornerShape(percent = 50),
            colors = ButtonDefaults.buttonColors(
              backgroundColor = if (!isRss) MaterialTheme.colors.primary else MaterialTheme.colors.surface.copy(alpha = 0.5f),
              contentColor = if (!isRss) MaterialTheme.colors.onPrimary else MaterialTheme.colors.onSurface
            ),
            elevation = null
          ) {
            Text("Mastodon", fontSize = 12.sp, fontWeight = FontWeight.Medium)
          }
        }

        OutlinedTextField(
          value = url,
          onValueChange = { url = it },
          label = { Text("Feed URL") },
          placeholder = {
            Text(if (feedType == FeedType.RSS_ATOM) "https://site.com/rss.xml" else "https://mastodon.social/api/v1/accounts/123/statuses")
          },
          singleLine = true,
          modifier = Modifier.fillMaxWidth(),
          shape = RoundedCornerShape(16.dp),
          colors = TextFieldDefaults.outlinedTextFieldColors(
            focusedBorderColor = MaterialTheme.colors.primary,
            cursorColor = MaterialTheme.colors.primary
          )
        )

        OutlinedTextField(
          value = title,
          onValueChange = { title = it },
          label = { Text("Title (Optional)") },
          placeholder = { Text("News / Updates") },
          singleLine = true,
          modifier = Modifier.fillMaxWidth(),
          shape = RoundedCornerShape(16.dp),
          colors = TextFieldDefaults.outlinedTextFieldColors(
            focusedBorderColor = MaterialTheme.colors.primary,
            cursorColor = MaterialTheme.colors.primary
          )
        )
      }
    },
    confirmButton = {
      Button(
        onClick = {
          if (url.isNotBlank()) {
            RichHaptics.perform(HapticPattern.SUCCESS)
            onAdd(url.trim(), title.trim(), feedType)
          }
        },
        enabled = url.isNotBlank(),
        shape = RoundedCornerShape(percent = 50),
        colors = ButtonDefaults.buttonColors(
          backgroundColor = MaterialTheme.colors.primary,
          contentColor = MaterialTheme.colors.onPrimary
        ),
        elevation = null
      ) {
        Text("Add")
      }
    },
    dismissButton = {
      TextButton(
        onClick = onDismiss,
        shape = RoundedCornerShape(percent = 50)
      ) {
        Text("Cancel", color = MaterialTheme.colors.primary)
      }
    }
  )
}
