package chat.simplex.common.views.usersettings

import SectionBottomSpacer
import SectionDividerSpaced
import androidx.compose.animation.*
import androidx.compose.animation.core.*
import androidx.compose.foundation.*
import androidx.compose.foundation.gestures.detectTapGestures
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.lazy.LazyRow
import androidx.compose.foundation.lazy.itemsIndexed
import androidx.compose.foundation.shape.CircleShape
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material.*
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.filled.Add
import androidx.compose.material.icons.filled.Check
import androidx.compose.material.icons.filled.Close
import androidx.compose.material.icons.filled.Delete
import androidx.compose.material.icons.filled.Image
import androidx.compose.material.icons.filled.Mic
import androidx.compose.material.icons.filled.Person
import androidx.compose.material.icons.filled.PlayArrow
import androidx.compose.material.icons.filled.Refresh
import androidx.compose.material.icons.filled.Schedule
import androidx.compose.material.icons.filled.Videocam
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.clip
import androidx.compose.ui.graphics.Brush
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.graphics.ImageBitmap
import androidx.compose.ui.graphics.lerp
import androidx.compose.ui.graphics.vector.rememberVectorPainter
import androidx.compose.ui.input.pointer.pointerInput
import androidx.compose.ui.layout.ContentScale
import androidx.compose.ui.text.font.FontFamily
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.style.TextAlign
import androidx.compose.ui.text.style.TextOverflow
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import androidx.compose.ui.window.Dialog
import androidx.compose.ui.window.DialogProperties
import chat.simplex.common.platform.*
import chat.simplex.common.views.helpers.*
import kotlinx.coroutines.delay
import kotlinx.coroutines.launch
import java.io.File

val LocalStoryVideoPlayer = compositionLocalOf<(@Composable (filePath: String, modifier: Modifier) -> Unit)?> { null }
val LocalStoryImageViewer = compositionLocalOf<(@Composable (filePath: String, modifier: Modifier) -> Unit)?> { null }

object TemporaryContentPickerBridge {
  var pickMedia: ((TemporaryMediaType, (String?) -> Unit) -> Unit)? = null
}

// =====================================================================
// 1. СЕКЦИЯ В ПРОФИЛЕ ("TEMPORARY CONTENT")
// =====================================================================
@Composable
fun TemporaryContentProfileSection() {
    val items by TemporaryContentManager.items.collectAsState()
    var showAddDialog by remember { mutableStateOf(false) }
    var viewingStartIndex by remember { mutableStateOf<Int?>(null) }
    var nowMillis by remember { mutableStateOf(System.currentTimeMillis()) }

    val myStories = remember(items) {
        TemporaryContentManager.getActiveItemsFor(null)
    }

    LaunchedEffect(Unit) {
        TemporaryContentManager.reload()
        StorySyncManager.syncAllStoriesNow(chatModel)
        while (true) {
            delay(15_000L)
            TemporaryContentManager.purgeExpired()
            nowMillis = System.currentTimeMillis()
        }
    }

    val isLight = MaterialTheme.colors.isLight
    val primary = MaterialTheme.colors.primary
    val cardBg = if (isLight) lerp(Color.White, primary, 0.055f) else lerp(MaterialTheme.colors.surface, primary, 0.12f)
    val borderColor = primary.copy(alpha = if (isLight) 0.16f else 0.24f)

    val handleAddClick = {
        if (myStories.size >= 4) {
            RichHaptics.perform(HapticPattern.WARNING)
            AlertManager.shared.showAlertMsg(
                "Limit reached",
                "You can publish a maximum of 4 Temporary Content items at the same time."
            )
        } else {
            RichHaptics.perform(HapticPattern.CLICK)
            showAddDialog = true
        }
    }

    Surface(
        shape = RoundedCornerShape(26.dp),
        color = cardBg,
        elevation = 0.dp,
        modifier = Modifier
            .fillMaxWidth()
            .padding(vertical = 4.dp)
            .border(0.75.dp, borderColor, RoundedCornerShape(26.dp))
    ) {
        Column(
            modifier = Modifier
                .fillMaxWidth()
                .padding(vertical = 14.dp)
        ) {
            Row(
                modifier = Modifier
                    .fillMaxWidth()
                    .padding(horizontal = 16.dp),
                verticalAlignment = Alignment.CenterVertically,
                horizontalArrangement = Arrangement.SpaceBetween
            ) {
                Row(verticalAlignment = Alignment.CenterVertically) {
                    Box(
                        modifier = Modifier
                            .size(36.dp)
                            .clip(RoundedCornerShape(12.dp))
                            .background(primary.copy(alpha = 0.15f)),
                        contentAlignment = Alignment.Center
                    ) {
                        Icon(
                            painter = rememberVectorPainter(Icons.Default.Schedule),
                            contentDescription = null,
                            tint = primary,
                            modifier = Modifier.size(20.dp)
                        )
                    }
                    Spacer(Modifier.width(12.dp))
                    Column {
                        Text(
                            text = "Temporary Content",
                            fontSize = 16.sp,
                            fontWeight = FontWeight.Bold,
                            color = MaterialTheme.colors.onSurface
                        )
                        Text(
                            text = if (myStories.isEmpty()) "Auto-expiring photos & videos" else "${myStories.size} active • auto-delete enabled",
                            fontSize = 12.sp,
                            color = MaterialTheme.colors.onSurface.copy(alpha = 0.6f)
                        )
                    }
                }

                Surface(
                    shape = CircleShape,
                    color = primary,
                    modifier = Modifier
                        .clip(CircleShape)
                        .clickable(onClick = handleAddClick)
                ) {
                    Row(
                        modifier = Modifier.padding(horizontal = 14.dp, vertical = 7.dp),
                        verticalAlignment = Alignment.CenterVertically
                    ) {
                        Icon(
                            painter = rememberVectorPainter(Icons.Default.Add),
                            contentDescription = "Add Temporary Content",
                            tint = MaterialTheme.colors.onPrimary,
                            modifier = Modifier.size(16.dp)
                        )
                        Spacer(Modifier.width(4.dp))
                        Text(
                            text = "Add",
                            fontSize = 13.sp,
                            fontWeight = FontWeight.SemiBold,
                            color = MaterialTheme.colors.onPrimary
                        )
                    }
                }
            }

            Spacer(Modifier.height(14.dp))

            LazyRow(
                contentPadding = PaddingValues(horizontal = 16.dp),
                horizontalArrangement = Arrangement.spacedBy(12.dp),
                verticalAlignment = Alignment.CenterVertically
            ) {
                item {
                    AddStoryCardTile(onClick = handleAddClick)
                }

                itemsIndexed(myStories, key = { _, item -> item.id }) { index, item ->
                    TemporaryStoryTile(
                        item = item,
                        nowMillis = nowMillis,
                        onClick = {
                            RichHaptics.perform(HapticPattern.CLICK)
                            viewingStartIndex = index
                        }
                    )
                }
            }
        }
    }
    
    if (showAddDialog) {
        AddTemporaryContentDialog(
            onDismiss = { showAddDialog = false },
            onPublished = {
                showAddDialog = false
                nowMillis = System.currentTimeMillis()
            }
        )
    }

    val startIdx = viewingStartIndex
    if (startIdx != null && myStories.isNotEmpty()) {
        val scope = rememberCoroutineScope()
        TemporaryContentStoryViewer(
            items = myStories,
            initialIndex = startIdx.coerceIn(0, myStories.lastIndex),
            onDismiss = { viewingStartIndex = null },
            onDeleteStory = { idToDelete ->
                val isMyStory = TemporaryContentManager.items.value.find { it.id == idToDelete }?.authorContactId == null
                TemporaryContentManager.removeContent(idToDelete)
                if (isMyStory) {
                    scope.launch { StorySyncManager.broadcastStoryDeletion(chatModel, idToDelete) }
                }
            }
        )
    }
}

// =====================================================================
// 2. ПЛИТКИ КАРУСЕЛИ
// =====================================================================
@Composable
private fun AddStoryCardTile(onClick: () -> Unit) {
  val primary = MaterialTheme.colors.primary
  Column(
    horizontalAlignment = Alignment.CenterHorizontally,
    modifier = Modifier.width(78.dp)
  ) {
    Box(
      modifier = Modifier
        .size(74.dp)
        .clip(RoundedCornerShape(22.dp))
        .background(primary.copy(alpha = 0.10f))
        .border(
          width = 1.5.dp,
          color = primary.copy(alpha = 0.35f),
          shape = RoundedCornerShape(22.dp)
        )
        .clickable(onClick = onClick),
      contentAlignment = Alignment.Center
    ) {
      Icon(
        painter = rememberVectorPainter(Icons.Default.Add),
        contentDescription = null,
        tint = primary,
        modifier = Modifier.size(28.dp)
      )
    }
    Spacer(Modifier.height(5.dp))
    Text(
      text = "New",
      fontSize = 11.5.sp,
      fontWeight = FontWeight.Medium,
      color = MaterialTheme.colors.onSurface.copy(alpha = 0.75f)
    )
  }
}

@Composable
private fun TemporaryStoryTile(
  item: TemporaryContentItem,
  nowMillis: Long,
  onClick: () -> Unit
) {
  val primary = MaterialTheme.colors.primary
  val ringBrush = Brush.linearGradient(
    colors = listOf(primary, lerp(primary, Color(0xFFFF6D00), 0.35f))
  )

  val thumbnailBitmap = remember(item.filePath) {
    loadStoryThumbnailSafe(item.filePath)
  }

  Column(
    horizontalAlignment = Alignment.CenterHorizontally,
    modifier = Modifier.width(78.dp)
  ) {
    Box(
      modifier = Modifier
        .size(74.dp)
        .clip(RoundedCornerShape(22.dp))
        .border(2.5.dp, ringBrush, RoundedCornerShape(22.dp))
        .padding(3.5.dp)
        .clip(RoundedCornerShape(18.5.dp))
        .background(Color(0xFF1E1E24))
        .clickable(onClick = onClick),
      contentAlignment = Alignment.Center
    ) {
      if (thumbnailBitmap != null) {
        Image(
          bitmap = thumbnailBitmap,
          contentDescription = null,
          contentScale = ContentScale.Crop,
          modifier = Modifier.fillMaxSize()
        )
      } else {
        Icon(
          painter = rememberVectorPainter(
            if (item.mediaType == TemporaryMediaType.VIDEO) Icons.Default.Mic else Icons.Default.Image
          ),
          contentDescription = null,
          tint = Color.White.copy(alpha = 0.85f),
          modifier = Modifier.size(26.dp)
        )
      }

      if (item.mediaType == TemporaryMediaType.VIDEO) {
        Box(
          modifier = Modifier
            .align(Alignment.TopEnd)
            .padding(4.dp)
            .size(18.dp)
            .clip(CircleShape)
            .background(Color.Black.copy(alpha = 0.55f)),
          contentAlignment = Alignment.Center
        ) {
          Icon(
            painter = rememberVectorPainter(Icons.Default.PlayArrow),
            contentDescription = null,
            tint = Color.White,
            modifier = Modifier.size(12.dp)
          )
        }
      }
    }

    Spacer(Modifier.height(5.dp))

    Text(
      text = item.remainingTimeText(nowMillis),
      fontSize = 11.sp,
      fontWeight = FontWeight.SemiBold,
      fontFamily = FontFamily.Monospace,
      color = primary,
      maxLines = 1
    )
  }
}

// =====================================================================
// 3. ДИАЛОГ ДОБАВЛЕНИЯ С ОТОБРАЖЕНИЕМ ПРОЦЕНТОВ ОТПРАВКИ
// =====================================================================
@Composable
private fun AddTemporaryContentDialog(
  onDismiss: () -> Unit,
  onPublished: () -> Unit
) {
  var selectedDuration by remember { mutableStateOf(TemporaryDuration.HOURS_24) }
  var selectedMediaType by remember { mutableStateOf(TemporaryMediaType.PHOTO) }
  var selectedAudience by remember { mutableStateOf(TemporaryAudience.CONTACTS) }
  var selectedFilePath by remember { mutableStateOf<String?>(null) }
  var caption by remember { mutableStateOf("") }

  var isUploading by remember { mutableStateOf(false) }
  var uploadProgress by remember { mutableStateOf(0) }
  var uploadStatus by remember { mutableStateOf("") }
  val coroutineScope = rememberCoroutineScope()

  val primary = MaterialTheme.colors.primary
  val isLight = MaterialTheme.colors.isLight
  val surfaceColor = if (isLight) lerp(Color.White, primary, 0.06f) else lerp(MaterialTheme.colors.surface, primary, 0.12f)

  Dialog(onDismissRequest = { if (!isUploading) onDismiss() }) {
    Surface(
      shape = RoundedCornerShape(28.dp),
      color = surfaceColor,
      elevation = 8.dp,
      modifier = Modifier
        .fillMaxWidth()
        .border(1.dp, primary.copy(alpha = 0.2f), RoundedCornerShape(28.dp))
    ) {
      Column(
        modifier = Modifier
          .fillMaxWidth()
          .padding(22.dp)
      ) {
        Text(
          text = "New Temporary Content",
          fontSize = 20.sp,
          fontWeight = FontWeight.Bold,
          color = MaterialTheme.colors.onSurface
        )
        Spacer(Modifier.height(4.dp))
        Text(
          text = "Select photo or video and privacy settings",
          fontSize = 13.sp,
          color = MaterialTheme.colors.onSurface.copy(alpha = 0.65f)
        )

        Spacer(Modifier.height(16.dp))

        Row(
          modifier = Modifier.fillMaxWidth(),
          horizontalArrangement = Arrangement.spacedBy(10.dp)
        ) {
          TemporaryMediaType.values().forEach { type ->
            val isSelected = selectedMediaType == type
            Surface(
              shape = RoundedCornerShape(16.dp),
              color = if (isSelected) primary else primary.copy(alpha = 0.10f),
              modifier = Modifier
                .weight(1f)
                .clip(RoundedCornerShape(16.dp))
                .clickable(enabled = !isUploading) {
                  RichHaptics.perform(HapticPattern.SELECTION_CHANGE)
                  selectedMediaType = type
                  selectedFilePath = null
                }
            ) {
              Row(
                modifier = Modifier.padding(vertical = 12.dp),
                horizontalArrangement = Arrangement.Center,
                verticalAlignment = Alignment.CenterVertically
              ) {
                Icon(
                  painter = rememberVectorPainter(
                    if (type == TemporaryMediaType.PHOTO) Icons.Default.Image else Icons.Default.Mic
                  ),
                  contentDescription = null,
                  tint = if (isSelected) MaterialTheme.colors.onPrimary else primary,
                  modifier = Modifier.size(18.dp)
                )
                Spacer(Modifier.width(8.dp))
                Text(
                  text = if (type == TemporaryMediaType.PHOTO) "Photo" else "Video",
                  fontSize = 14.sp,
                  fontWeight = FontWeight.SemiBold,
                  color = if (isSelected) MaterialTheme.colors.onPrimary else primary
                )
              }
            }
          }
        }

        Spacer(Modifier.height(14.dp))

        Surface(
          shape = RoundedCornerShape(18.dp),
          color = primary.copy(alpha = 0.08f),
          modifier = Modifier
            .fillMaxWidth()
            .border(1.dp, primary.copy(alpha = 0.28f), RoundedCornerShape(18.dp))
            .clip(RoundedCornerShape(18.dp))
            .clickable(enabled = !isUploading) {
              RichHaptics.perform(HapticPattern.CLICK)
              TemporaryContentPickerBridge.pickMedia?.invoke(selectedMediaType) { path ->
                selectedFilePath = path
              }
            }
        ) {
          Row(
            modifier = Modifier
              .fillMaxWidth()
              .padding(vertical = 16.dp, horizontal = 16.dp),
            verticalAlignment = Alignment.CenterVertically,
            horizontalArrangement = Arrangement.Center
          ) {
            Icon(
              painter = rememberVectorPainter(if (selectedFilePath != null) Icons.Default.Check else Icons.Default.Image),
              contentDescription = null,
              tint = primary,
              modifier = Modifier.size(22.dp)
            )
            Spacer(Modifier.width(10.dp))
            Text(
              text = if (selectedFilePath != null) {
                val kb = try { File(selectedFilePath!!).length() / 1024 } catch (e: Exception) { 0 }
                "Media selected ($kb KB)"
              } else {
                "Tap to choose ${if (selectedMediaType == TemporaryMediaType.PHOTO) "Photo" else "Video"}"
              },
              fontSize = 14.sp,
              fontWeight = FontWeight.SemiBold,
              color = primary
            )
          }
        }

        Spacer(Modifier.height(16.dp))

        Text(
          text = "SHARE WITH",
          fontSize = 11.sp,
          fontWeight = FontWeight.Bold,
          letterSpacing = 1.sp,
          color = primary
        )
        Spacer(Modifier.height(8.dp))
        Row(
          modifier = Modifier.fillMaxWidth(),
          horizontalArrangement = Arrangement.spacedBy(6.dp)
        ) {
          TemporaryAudience.values().forEach { aud ->
            val isSelected = selectedAudience == aud
            Surface(
              shape = RoundedCornerShape(percent = 50),
              color = if (isSelected) primary else primary.copy(alpha = 0.10f),
              modifier = Modifier
                .weight(1f)
                .clip(RoundedCornerShape(percent = 50))
                .clickable(enabled = !isUploading) {
                  RichHaptics.perform(HapticPattern.SELECTION_CHANGE)
                  selectedAudience = aud
                }
            ) {
              Text(
                text = aud.label,
                fontSize = 11.5.sp,
                fontWeight = if (isSelected) FontWeight.Bold else FontWeight.Medium,
                color = if (isSelected) MaterialTheme.colors.onPrimary else MaterialTheme.colors.onSurface,
                textAlign = TextAlign.Center,
                maxLines = 1,
                overflow = TextOverflow.Ellipsis,
                modifier = Modifier.padding(vertical = 8.dp, horizontal = 4.dp)
              )
            }
          }
        }

        Spacer(Modifier.height(16.dp))

        Text(
          text = "AUTO-DELETE AFTER",
          fontSize = 11.sp,
          fontWeight = FontWeight.Bold,
          letterSpacing = 1.sp,
          color = primary
        )
        Spacer(Modifier.height(8.dp))
        
        val durations = TemporaryDuration.values().toList()
        Column(verticalArrangement = Arrangement.spacedBy(6.dp)) {
          durations.chunked(3).forEach { rowItems ->
            Row(
              modifier = Modifier.fillMaxWidth(),
              horizontalArrangement = Arrangement.spacedBy(6.dp)
            ) {
              rowItems.forEach { duration ->
                val isSelected = selectedDuration == duration
                Surface(
                  shape = RoundedCornerShape(percent = 50),
                  color = if (isSelected) primary else primary.copy(alpha = 0.10f),
                  modifier = Modifier
                    .weight(1f)
                    .clip(RoundedCornerShape(percent = 50))
                    .clickable(enabled = !isUploading) {
                      RichHaptics.perform(HapticPattern.SELECTION_CHANGE)
                      selectedDuration = duration
                    }
                ) {
                  Text(
                    text = duration.label,
                    fontSize = 12.sp,
                    fontWeight = if (isSelected) FontWeight.Bold else FontWeight.Medium,
                    color = if (isSelected) MaterialTheme.colors.onPrimary else MaterialTheme.colors.onSurface,
                    textAlign = TextAlign.Center,
                    modifier = Modifier.padding(vertical = 8.dp)
                  )
                }
              }
              repeat(3 - rowItems.size) {
                Spacer(modifier = Modifier.weight(1f))
              }
            }
          }
        }

        Spacer(Modifier.height(14.dp))

        OutlinedTextField(
          value = caption,
          onValueChange = { if (it.length <= 140) caption = it },
          placeholder = { Text("Add a caption (optional)...", fontSize = 13.sp) },
          singleLine = true,
          enabled = !isUploading,
          shape = RoundedCornerShape(14.dp),
          colors = TextFieldDefaults.outlinedTextFieldColors(
            focusedBorderColor = primary,
            unfocusedBorderColor = primary.copy(alpha = 0.25f)
          ),
          modifier = Modifier.fillMaxWidth()
        )

        Spacer(Modifier.height(20.dp))

        if (isUploading) {
          Column(
            modifier = Modifier.fillMaxWidth(),
            horizontalAlignment = Alignment.CenterHorizontally
          ) {
            LinearProgressIndicator(
              progress = (uploadProgress.toFloat() / 100f).coerceIn(0f, 1f),
              color = primary,
              backgroundColor = primary.copy(alpha = 0.2f),
              modifier = Modifier
                .fillMaxWidth()
                .height(6.dp)
                .clip(CircleShape)
            )
            Spacer(Modifier.height(10.dp))
            Text(
              text = "$uploadStatus ($uploadProgress%)",
              fontSize = 13.5.sp,
              fontWeight = FontWeight.Bold,
              color = primary
            )
          }
        } else {
          Row(
            modifier = Modifier.fillMaxWidth(),
            horizontalArrangement = Arrangement.End,
            verticalAlignment = Alignment.CenterVertically
          ) {
            TextButton(
              onClick = onDismiss,
              shape = CircleShape
            ) {
              Text("Cancel", color = primary, fontWeight = FontWeight.SemiBold)
            }
            Spacer(Modifier.width(8.dp))
            Button(
              onClick = {
                val rawPath = selectedFilePath ?: return@Button
                RichHaptics.perform(HapticPattern.CLICK)

                val newItem = TemporaryContentManager.addContentFromFilePath(
                  sourcePath = rawPath,
                  mediaType = selectedMediaType,
                  duration = selectedDuration,
                  caption = caption.trim(),
                  audience = selectedAudience
                ) ?: return@Button

                isUploading = true
                coroutineScope.launch {
                  val mediaFile = File(newItem.filePath)
                  val success = StorySyncManager.publishAndBroadcastStoryWithProgress(
                    file = mediaFile,
                    item = newItem
                  ) { pct, status ->
                    uploadProgress = pct
                    uploadStatus = status
                  }

                  if (success) {
                    RichHaptics.perform(HapticPattern.SUCCESS)
                    delay(300L)
                    onPublished()
                  } else {
                    RichHaptics.perform(HapticPattern.ERROR)
                    uploadStatus = "Failed to upload"
                    delay(1500L)
                    isUploading = false
                  }
                }
              },
              enabled = selectedFilePath != null,
              shape = CircleShape,
              colors = ButtonDefaults.buttonColors(
                backgroundColor = primary,
                contentColor = MaterialTheme.colors.onPrimary
              ),
              elevation = null
            ) {
              Text("Publish", fontWeight = FontWeight.Bold)
            }
          }
        }
      }
    }
  }
}

// =====================================================================
// 4. ПОЛНОЭКРАННЫЙ ПРОСМОТРЩИК TEMPORARY CONTENT (MATERIAL 3 EXPRESSIVE)
// =====================================================================
@Composable
fun TemporaryContentStoryViewer(
    items: List<TemporaryContentItem>,
    initialIndex: Int,
    onDismiss: () -> Unit,
    onDeleteStory: (String) -> Unit = {}
) {
    var currentIndex by remember { mutableStateOf(initialIndex.coerceIn(0, items.lastIndex)) }
    val currentItem = items.getOrNull(currentIndex) ?: run {
        onDismiss()
        return
    }

    val now = System.currentTimeMillis()

    Dialog(
        onDismissRequest = onDismiss,
        properties = DialogProperties(usePlatformDefaultWidth = false)
    ) {
        Box(
            modifier = Modifier
                .fillMaxSize()
                .background(Color.Black)
                .pointerInput(currentIndex, items.size) {
                    detectTapGestures { offset ->
                        if (offset.x < size.width * 0.35f) {
                            if (currentIndex > 0) {
                                RichHaptics.perform(HapticPattern.TICK)
                                currentIndex--
                            }
                        } else {
                            if (currentIndex < items.lastIndex) {
                                RichHaptics.perform(HapticPattern.TICK)
                                currentIndex++
                            } else {
                                onDismiss()
                            }
                        }
                    }
                }
        ) {
            val mediaFile = remember(currentItem.filePath) { File(currentItem.filePath) }
            val isFileReady = mediaFile.exists() && mediaFile.length() > 1000L

            // 1. Медиа-контент
            if (currentItem.mediaType == TemporaryMediaType.VIDEO) {
                val videoPlayer = LocalStoryVideoPlayer.current
                if (videoPlayer != null && isFileReady) {
                    videoPlayer(currentItem.filePath, Modifier.fillMaxSize())
                } else {
                    Box(Modifier.fillMaxSize(), contentAlignment = Alignment.Center) {
                        CircularProgressIndicator(color = Color.White)
                    }
                }
            } else {
                val imageViewer = LocalStoryImageViewer.current
                if (imageViewer != null && isFileReady) {
                    imageViewer(currentItem.filePath, Modifier.fillMaxSize())
                } else {
                    Box(Modifier.fillMaxSize(), contentAlignment = Alignment.Center) {
                        CircularProgressIndicator(color = Color.White)
                    }
                }
            }

            // 2. Верхняя панель со статусом и таймером (Плавающий M3-контейнер)
            Column(
                modifier = Modifier
                    .fillMaxWidth()
                    .align(Alignment.TopCenter)
                    .statusBarsPadding()
                    .padding(top = 16.dp, start = 16.dp, end = 16.dp)
            ) {
                // Индикаторы прогресса
                Row(
                    modifier = Modifier.fillMaxWidth(),
                    horizontalArrangement = Arrangement.spacedBy(6.dp)
                ) {
                    items.forEachIndexed { idx, it ->
                        val progress = when {
                            idx < currentIndex -> 1f
                            idx == currentIndex -> it.remainingProgress(now)
                            else -> 0f
                        }
                        LinearProgressIndicator(
                            progress = progress,
                            color = Color.White,
                            backgroundColor = Color.White.copy(alpha = 0.3f),
                            modifier = Modifier
                                .weight(1f)
                                .height(4.dp)
                                .clip(CircleShape)
                        )
                    }
                }

                Spacer(Modifier.height(14.dp))

                // Информация об авторе и кнопки
                Row(
                    modifier = Modifier.fillMaxWidth(),
                    verticalAlignment = Alignment.CenterVertically,
                    horizontalArrangement = Arrangement.SpaceBetween
                ) {
                    // Остров с таймером и именем
                    Surface(
                        color = Color.Black.copy(alpha = 0.35f),
                        shape = RoundedCornerShape(percent = 50),
                        elevation = 0.dp
                    ) {
                        Row(
                            verticalAlignment = Alignment.CenterVertically,
                            modifier = Modifier.padding(horizontal = 12.dp, vertical = 6.dp)
                        ) {
                            Icon(rememberVectorPainter(Icons.Default.Schedule), null, tint = Color.White, modifier = Modifier.size(16.dp))
                            Spacer(Modifier.width(6.dp))
                            val authorLabel = currentItem.authorDisplayName ?: "Contact"
                            Text("$authorLabel • ${currentItem.remainingTimeText(now)}", color = Color.White, fontSize = 13.sp, fontWeight = FontWeight.Bold)
                        }
                    }

                    // Острова с кнопками управления
                    Row(verticalAlignment = Alignment.CenterVertically, horizontalArrangement = Arrangement.spacedBy(8.dp)) {
                        Surface(
                            color = Color.Black.copy(alpha = 0.35f),
                            shape = CircleShape,
                            modifier = Modifier.size(36.dp)
                        ) {
                            IconButton(
                                onClick = {
                                    RichHaptics.perform(HapticPattern.TOGGLE_OFF)
                                    onDeleteStory(currentItem.id)
                                    if (items.size <= 1) onDismiss()
                                }
                            ) {
                                Icon(rememberVectorPainter(Icons.Default.Delete), null, tint = Color(0xFFFF5252), modifier = Modifier.size(18.dp))
                            }
                        }

                        Surface(
                            color = Color.Black.copy(alpha = 0.35f),
                            shape = CircleShape,
                            modifier = Modifier.size(36.dp)
                        ) {
                            IconButton(onClick = onDismiss) {
                                Icon(rememberVectorPainter(Icons.Default.Close), null, tint = Color.White, modifier = Modifier.size(18.dp))
                            }
                        }
                    }
                }
            }

            // 3. Плавающая подпись снизу (Плавающий M3-контейнер)
            if (currentItem.caption.isNotBlank()) {
                Box(
                    modifier = Modifier
                        .fillMaxWidth()
                        .align(Alignment.BottomCenter)
                        .navigationBarsPadding()
                        .padding(horizontal = 24.dp, vertical = 24.dp),
                    contentAlignment = Alignment.Center
                ) {
                    Surface(
                        color = Color.Black.copy(alpha = 0.45f),
                        shape = RoundedCornerShape(16.dp),
                        elevation = 0.dp
                    ) {
                        Text(
                            text = currentItem.caption,
                            color = Color.White,
                            fontSize = 15.sp,
                            textAlign = TextAlign.Center,
                            maxLines = 4,
                            modifier = Modifier.padding(horizontal = 16.dp, vertical = 12.dp)
                        )
                    }
                }
            }
        }
    }
}

// =====================================================================
// 5. ГЛАВНЫЙ ЭКРАН ВСЕХ ИСТОРИЙ (ДЛЯ СПИСКА ЧАТОВ)
// =====================================================================
@Composable
fun AllStoriesFeedView(close: () -> Unit) {
    val items by TemporaryContentManager.items.collectAsState()
    var showAddDialog by remember { mutableStateOf(false) }
    var nowMillis by remember { mutableStateOf(System.currentTimeMillis()) }
    var viewingStories by remember { mutableStateOf<List<TemporaryContentItem>?>(null) }
    var isReloading by remember { mutableStateOf(false) }
    val scope = rememberCoroutineScope()

    LaunchedEffect(Unit) {
        TemporaryContentManager.reload()
        StorySyncManager.syncAllStoriesNow(chatModel)
        while (true) {
            delay(8_000L)
            StorySyncManager.syncAllStoriesNow(chatModel)
            nowMillis = System.currentTimeMillis()
        }
    }

    val myStories = items.filter { it.authorContactId == null }.sortedBy { it.createdAtMillis }
    val contactStories = items.filter { it.authorContactId != null }
        .groupBy { it.authorContactId }
        .values.map { list -> list.sortedBy { it.createdAtMillis } }
        .sortedByDescending { it.last().createdAtMillis }

    val primary = MaterialTheme.colors.primary

    val handleAddClick = {
        if (myStories.size >= 4) {
            RichHaptics.perform(HapticPattern.WARNING)
            AlertManager.shared.showAlertMsg(
                "Limit reached",
                "You can publish a maximum of 4 Temporary Content items at the same time."
            )
        } else {
            RichHaptics.perform(HapticPattern.CLICK)
            showAddDialog = true
        }
    }

    ModalView(close = close, cardScreen = true) {
        ColumnWithScrollBar {
            Column(
                modifier = Modifier
                    .fillMaxWidth()
                    .padding(horizontal = 16.dp, vertical = 12.dp)
            ) {
                Text(
                    text = "Temporary Content",
                    style = MaterialTheme.typography.h1,
                    fontWeight = FontWeight.Bold,
                    color = MaterialTheme.colors.onSurface
                )

                Spacer(Modifier.height(8.dp))

                OutlinedButton(
                    onClick = {
                        if (!isReloading) {
                            isReloading = true
                            RichHaptics.perform(HapticPattern.CLICK)
                            scope.launch {
                                TemporaryContentManager.reload() // Перезагружаем при ручном обновлении
                                StorySyncManager.syncAllStoriesNow(chatModel)
                                delay(600L)
                                isReloading = false
                                RichHaptics.perform(HapticPattern.SUCCESS)
                            }
                        }
                    },
                    shape = CircleShape,
                    border = BorderStroke(1.dp, primary.copy(alpha = 0.35f)),
                    colors = ButtonDefaults.outlinedButtonColors(
                        backgroundColor = primary.copy(alpha = if (isReloading) 0.15f else 0.05f)
                    ),
                    contentPadding = PaddingValues(horizontal = 14.dp, vertical = 6.dp)
                ) {
                    if (isReloading) {
                        CircularProgressIndicator(
                            color = primary,
                            strokeWidth = 2.dp,
                            modifier = Modifier.size(16.dp)
                        )
                    } else {
                        Icon(
                            imageVector = Icons.Default.Refresh,
                            contentDescription = "Reload",
                            tint = primary,
                            modifier = Modifier.size(16.dp)
                        )
                    }
                    Spacer(Modifier.width(6.dp))
                    Text(
                        text = if (isReloading) "Updating..." else "Reload",
                        fontSize = 13.sp,
                        fontWeight = FontWeight.SemiBold,
                        color = primary
                    )
                }
            }

            Surface(
                shape = RoundedCornerShape(24.dp),
                color = primary.copy(alpha = 0.05f),
                modifier = Modifier.fillMaxWidth().padding(horizontal = 16.dp, vertical = 8.dp)
            ) {
                Row(
                    modifier = Modifier.clickable {
                        if (myStories.isNotEmpty()) {
                            viewingStories = myStories
                        } else {
                            handleAddClick()
                        }
                    }.padding(16.dp),
                    verticalAlignment = Alignment.CenterVertically
                ) {
                    Box(
                        modifier = Modifier
                            .size(56.dp)
                            .clip(CircleShape)
                            .background(if (myStories.isNotEmpty()) primary else primary.copy(alpha = 0.15f))
                            .border(2.dp, primary.copy(alpha = 0.3f), CircleShape),
                        contentAlignment = Alignment.Center
                    ) {
                        Icon(
                            painter = rememberVectorPainter(if (myStories.isNotEmpty()) Icons.Default.PlayArrow else Icons.Default.Add),
                            contentDescription = null,
                            tint = if (myStories.isNotEmpty()) Color.White else primary,
                            modifier = Modifier.size(24.dp)
                        )
                    }
                    Spacer(Modifier.width(16.dp))
                    Column(modifier = Modifier.weight(1f)) {
                        Text(
                            text = "My Content",
                            fontSize = 17.sp,
                            fontWeight = FontWeight.Bold,
                            color = MaterialTheme.colors.onSurface
                        )
                        Text(
                            text = if (myStories.isNotEmpty()) "${myStories.size} active • Tap to view" else "Tap to add photo or video",
                            fontSize = 13.sp,
                            color = MaterialTheme.colors.onSurface.copy(alpha = 0.65f)
                        )
                    }
                    if (myStories.isNotEmpty()) {
                        IconButton(
                            onClick = handleAddClick,
                            modifier = Modifier.background(primary.copy(alpha = 0.1f), CircleShape)
                        ) {
                            Icon(rememberVectorPainter(Icons.Default.Add), null, tint = primary)
                        }
                    }
                }
            }

            Divider(
                modifier = Modifier.padding(vertical = 12.dp),
                color = MaterialTheme.colors.onSurface.copy(alpha = 0.08f)
            )

            Text(
                text = "RECENT UPDATES",
                fontSize = 12.sp,
                fontWeight = FontWeight.Bold,
                letterSpacing = 1.sp,
                color = primary,
                modifier = Modifier.padding(horizontal = 24.dp, vertical = 8.dp)
            )

            if (contactStories.isEmpty()) {
                Surface(
                    shape = RoundedCornerShape(20.dp),
                    color = primary.copy(alpha = 0.04f),
                    modifier = Modifier
                        .fillMaxWidth()
                        .padding(horizontal = 16.dp, vertical = 6.dp)
                ) {
                    Box(
                        modifier = Modifier
                            .fillMaxWidth()
                            .padding(24.dp),
                        contentAlignment = Alignment.Center
                    ) {
                        Text(
                            text = "No updates from contacts yet",
                            color = MaterialTheme.colors.secondary,
                            fontSize = 14.sp
                        )
                    }
                }
            } else {
                contactStories.forEach { userStories ->
                    val authorName = userStories.first().authorDisplayName ?: "Contact"
                    Row(
                        modifier = Modifier
                            .fillMaxWidth()
                            .clickable { viewingStories = userStories }
                            .padding(horizontal = 16.dp, vertical = 10.dp),
                        verticalAlignment = Alignment.CenterVertically
                    ) {
                        Box(
                            modifier = Modifier
                                .size(56.dp)
                                .clip(CircleShape)
                                .border(2.5.dp, Brush.linearGradient(listOf(primary, Color(0xFFFF6D00))), CircleShape)
                                .padding(4.dp)
                                .background(Color(0xFF1E1E24), CircleShape),
                            contentAlignment = Alignment.Center
                        ) {
                            Icon(
                                painter = rememberVectorPainter(Icons.Default.Person),
                                contentDescription = null,
                                tint = Color.White.copy(alpha = 0.7f),
                                modifier = Modifier.size(28.dp)
                            )
                        }
                        Spacer(Modifier.width(16.dp))
                        Column(modifier = Modifier.weight(1f)) {
                            Text(
                                text = authorName,
                                fontSize = 17.sp,
                                fontWeight = FontWeight.SemiBold,
                                color = MaterialTheme.colors.onSurface
                            )
                            Text(
                                text = "${userStories.size} active • expires in ${userStories.first().remainingTimeText(nowMillis)}",
                                fontSize = 13.sp,
                                color = MaterialTheme.colors.onSurface.copy(alpha = 0.65f)
                            )
                        }
                    }
                }
            }

            Spacer(Modifier.navigationBarsPadding().height(24.dp))
        }
    }

    if (showAddDialog) {
        AddTemporaryContentDialog(
            onDismiss = { showAddDialog = false },
            onPublished = {
                showAddDialog = false
                nowMillis = System.currentTimeMillis()
            }
        )
    }

    viewingStories?.let { storiesToView ->
        TemporaryContentStoryViewer(
            items = storiesToView,
            initialIndex = 0,
            onDismiss = { viewingStories = null },
            onDeleteStory = { idToDelete -> 
                val isMyStory = TemporaryContentManager.items.value.find { it.id == idToDelete }?.authorContactId == null
                TemporaryContentManager.removeContent(idToDelete)
                if (isMyStory) {
                    scope.launch { StorySyncManager.broadcastStoryDeletion(chatModel, idToDelete) }
                }
            }
        )
    }
}

private fun loadStoryThumbnailSafe(filePath: String): ImageBitmap? {
  return try {
    val thumbFile = File("${filePath}_thumb.jpg")
    val targetFile = if (thumbFile.exists() && thumbFile.length() > 0) thumbFile else File(filePath)
    
    if (targetFile.exists() && targetFile.length() in 1..900_000L) {
      val bytes = targetFile.readBytes()
      val base64Str = java.util.Base64.getEncoder().encodeToString(bytes)
      base64ToBitmap(base64Str)
    } else null
  } catch (_: Exception) {
    null
  }
}
