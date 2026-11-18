package chat.simplex.common.views.usersettings

import androidx.compose.foundation.clickable
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.rememberScrollState
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.foundation.verticalScroll
import androidx.compose.material.*
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.filled.ChevronRight
import androidx.compose.material.icons.filled.Lock
import androidx.compose.material.icons.filled.Notifications
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.graphics.painter.Painter
import androidx.compose.ui.graphics.vector.rememberVectorPainter
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.style.TextAlign
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import chat.simplex.common.model.ChatModel
import chat.simplex.common.platform.HapticPattern
import chat.simplex.common.platform.RichHaptics
import chat.simplex.common.platform.UsbPanicDetector
import chat.simplex.res.MR
import chat.simplex.res.*

// =====================================================================
// 1. ENTRY TILE FOR SETTINGS MENU
// =====================================================================
@Composable
fun FlexSettingsEntryTile(onClick: () -> Unit) {
  Card(
    shape = RoundedCornerShape(16.dp),
    backgroundColor = MaterialTheme.colors.surface,
    elevation = 0.dp,
    modifier = Modifier
      .fillMaxWidth()
      .padding(horizontal = 16.dp, vertical = 6.dp)
      .clickable {
        RichHaptics.perform(HapticPattern.CLICK)
        onClick()
      }
  ) {
    Row(
      modifier = Modifier
        .fillMaxWidth()
        .padding(horizontal = 16.dp, vertical = 14.dp),
      verticalAlignment = Alignment.CenterVertically,
      horizontalArrangement = Arrangement.SpaceBetween
    ) {
      Row(verticalAlignment = Alignment.CenterVertically) {
        Icon(
          painter = rememberVectorPainter(Icons.Default.Lock),
          contentDescription = null,
          tint = MaterialTheme.colors.primary,
          modifier = Modifier.size(24.dp)
        )
        Spacer(modifier = Modifier.width(16.dp))
        Column {
          Text(
            text = "FLEX Settings",
            fontSize = 16.sp,
            fontWeight = FontWeight.Medium,
            color = MaterialTheme.colors.onSurface
          )
          Text(
            text = "Custom security & modifications",
            fontSize = 12.sp,
            color = MaterialTheme.colors.onSurface.copy(alpha = 0.6f)
          )
        }
      }
      Icon(
        painter = rememberVectorPainter(Icons.Default.ChevronRight),
        contentDescription = null,
        tint = MaterialTheme.colors.onSurface.copy(alpha = 0.4f),
        modifier = Modifier.size(18.dp)
      )
    }
  }
}

// =====================================================================
// 2. FLEX SETTINGS SCREEN
// =====================================================================
@Composable
fun FlexSettingsView(chatModel: ChatModel? = null) {
  var usbPanicEnabled by remember { mutableStateOf(UsbPanicDetector.isEnabled()) }
  var advancedHapticsEnabled by remember { mutableStateOf(RichHaptics.isEnabled()) }
  val bottomPadding = WindowInsets.navigationBars.asPaddingValues().calculateBottomPadding() + 90.dp

  Column(
    modifier = Modifier
      .fillMaxSize()
      .verticalScroll(rememberScrollState())
      .padding(horizontal = 16.dp)
  ) {
    Spacer(Modifier.height(18.dp))

    Text(
      text = "FLEX Settings",
      fontSize = 22.sp,
      fontWeight = FontWeight.Bold,
      color = MaterialTheme.colors.onBackground,
      textAlign = TextAlign.Center,
      modifier = Modifier
        .fillMaxWidth()
        .padding(vertical = 14.dp)
    )

    Spacer(Modifier.height(8.dp))

    // SECTION 1: SECURITY
    FlexCategoryHeader("SECURITY")

    FlexSettingCard {
      FlexToggleItem(
        title = "USB Panic",
        subtitle = "Purge cache, clipboard and kill app on USB plug",
        icon = rememberVectorPainter(Icons.Default.Lock),
        iconTint = if (usbPanicEnabled) Color.Red else MaterialTheme.colors.onSurface.copy(alpha = 0.6f),
        checked = usbPanicEnabled,
        activeTrackColor = Color.Red,
        onCheckedChange = { enabled ->
          usbPanicEnabled = enabled
          UsbPanicDetector.setEnabled(enabled)
          if (advancedHapticsEnabled) {
            RichHaptics.perform(if (enabled) HapticPattern.WARNING else HapticPattern.TOGGLE_OFF)
          }
        }
      )
    }

    Spacer(modifier = Modifier.height(20.dp))

    // SECTION 2: INTERFACE & FEEDBACK
    FlexCategoryHeader("INTERFACE & FEEDBACK")

    FlexSettingCard {
      FlexToggleItem(
        title = "Advanced Haptics",
        subtitle = "Tactile vibration and feedback across UI",
        icon = rememberVectorPainter(Icons.Default.Notifications),
        iconTint = if (advancedHapticsEnabled) Color(0xFFFFA000) else MaterialTheme.colors.onSurface.copy(alpha = 0.6f),
        checked = advancedHapticsEnabled,
        onCheckedChange = { enabled ->
          advancedHapticsEnabled = enabled
          RichHaptics.setEnabled(enabled)
          if (enabled) {
            RichHaptics.perform(HapticPattern.HEAVY_CLICK)
          }
        }
      )
    }

    Spacer(Modifier.height(bottomPadding))
  }
}

// =====================================================================
// 3. REUSABLE COMPONENTS
// =====================================================================

@Composable
fun FlexCategoryHeader(title: String) {
  Text(
    text = title,
    fontSize = 12.sp,
    fontWeight = FontWeight.Bold,
    color = MaterialTheme.colors.primary,
    modifier = Modifier.padding(bottom = 8.dp, start = 4.dp)
  )
}

@Composable
fun FlexSettingCard(content: @Composable ColumnScope.() -> Unit) {
  Card(
    shape = RoundedCornerShape(16.dp),
    backgroundColor = MaterialTheme.colors.surface,
    elevation = 0.dp,
    modifier = Modifier.fillMaxWidth()
  ) {
    Column(
      modifier = Modifier.padding(vertical = 4.dp),
      content = content
    )
  }
}

@Composable
fun FlexToggleItem(
  title: String,
  subtitle: String? = null,
  icon: Painter? = null,
  iconTint: Color = MaterialTheme.colors.onSurface.copy(alpha = 0.7f),
  checked: Boolean,
  activeTrackColor: Color = MaterialTheme.colors.primary,
  onCheckedChange: (Boolean) -> Unit
) {
  Row(
    modifier = Modifier
      .fillMaxWidth()
      .padding(horizontal = 16.dp, vertical = 12.dp),
    verticalAlignment = Alignment.CenterVertically,
    horizontalArrangement = Arrangement.SpaceBetween
  ) {
    Row(
      verticalAlignment = Alignment.CenterVertically,
      modifier = Modifier.weight(1f)
    ) {
      if (icon != null) {
        Icon(
          painter = icon,
          contentDescription = null,
          tint = iconTint,
          modifier = Modifier.size(24.dp)
        )
        Spacer(modifier = Modifier.width(16.dp))
      }
      Column {
        Text(
          text = title,
          fontSize = 16.sp,
          fontWeight = FontWeight.Medium,
          color = MaterialTheme.colors.onSurface
        )
        if (subtitle != null) {
          Text(
            text = subtitle,
            fontSize = 12.sp,
            color = MaterialTheme.colors.onSurface.copy(alpha = 0.6f)
          )
        }
      }
    }
    Switch(
      checked = checked,
      onCheckedChange = onCheckedChange,
      colors = SwitchDefaults.colors(
        checkedThumbColor = activeTrackColor,
        checkedTrackColor = activeTrackColor.copy(alpha = 0.5f)
      )
    )
  }
}
