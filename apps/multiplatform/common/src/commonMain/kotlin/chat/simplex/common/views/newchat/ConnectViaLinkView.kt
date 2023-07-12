package chat.simplex.common.views.newchat

import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import androidx.compose.ui.unit.sp
import chat.simplex.common.model.ChatModel
import chat.simplex.common.platform.AppPlatform
import chat.simplex.common.platform.appPlatform
import chat.simplex.res.MR

enum class ConnectViaLinkTab {
  SCAN, PASTE
}

@Composable
fun ConnectViaLinkView(m: ChatModel, close: () -> Unit) {
  val pages = remember {
    if (appPlatform.isAndroid) {
      ConnectViaLinkTab.values().toList()
    } else {
      listOf(ConnectViaLinkTab.PASTE)
    }
  }
  val selection = remember {
    mutableStateOf(
      if (appPlatform.isAndroid) {
        runCatching { ConnectViaLinkTab.valueOf(m.controller.appPrefs.connectViaLinkTab.get()!!) }.getOrDefault(ConnectViaLinkTab.SCAN)
      } else {
        ConnectViaLinkTab.PASTE
      }
    )
  }
  val tabTitles = pages.map {
    when (it) {
      ConnectViaLinkTab.SCAN -> stringResource(MR.strings.scan_QR_code)
      ConnectViaLinkTab.PASTE -> stringResource(MR.strings.paste_the_link_you_received)
    }
  }
  Column(
    Modifier.fillMaxHeight(),
    verticalArrangement = Arrangement.SpaceBetween
  ) {
    Column(Modifier.weight(1f)) {
      when (selection.value) {
        ConnectViaLinkTab.SCAN -> {
          ScanToConnectView(m, close)
        }
        ConnectViaLinkTab.PASTE -> {
          PasteToConnectView(m, close)
        }
      }
    }
    if (pages.size > 1) {
      TabRow(
        selectedTabIndex = pages.indexOf(selection.value),
        backgroundColor = Color.Transparent,
        contentColor = MaterialTheme.colors.primary,
      ) {
        tabTitles.forEachIndexed { index, it ->
          Tab(
            selected = index == tabTitles.indexOf(it),
            onClick = {
              selection.value = pages[index]
              m.controller.appPrefs.connectViaLinkTab.set(selection.value.name)
            },
            text = { Text(it, fontSize = 13.sp) },
            icon = {
              Icon(
                if (ConnectViaLinkTab.SCAN == pages[index]) painterResource(MR.images.ic_qr_code) else painterResource(MR.images.ic_article),
                it
              )
            },
            selectedContentColor = MaterialTheme.colors.primary,
            unselectedContentColor = MaterialTheme.colors.secondary,
          )
        }
      }
    }
  }
}
