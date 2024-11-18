package chat.simplex.common.views.usersettings.networkAndServers

import SectionItemView
import SectionTextFooter
import SectionView
import androidx.compose.foundation.Image
import androidx.compose.foundation.clickable
import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.alpha
import androidx.compose.ui.platform.LocalUriHandler
import androidx.compose.ui.text.intl.Locale
import androidx.compose.ui.unit.dp
import chat.simplex.common.model.*
import chat.simplex.common.platform.ColumnWithScrollBar
import chat.simplex.common.views.helpers.*
import chat.simplex.res.MR
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import kotlinx.datetime.*
import java.time.format.DateTimeFormatter
import java.time.format.FormatStyle

@Composable
fun OperatorView(currUserServers: MutableState<List<UserOperatorServers>>, userServers: MutableState<List<UserOperatorServers>>, operatorIndex: Int) {
  val testing = remember { mutableStateOf(false) }

  Box(Modifier.alpha(if (testing.value) 0.6f else 1f)) {
    OperatorViewLayout(currUserServers, userServers, operatorIndex)
    if (testing.value) {
      DefaultProgressView(null)
    }
  }
}

@Composable
fun OperatorViewLayout(currUserServers: MutableState<List<UserOperatorServers>>, userServers: MutableState<List<UserOperatorServers>>, operatorIndex: Int) {
  val operator = remember { userServers.value[operatorIndex].operator_ }

  Column {
    SectionView(generalGetString(MR.strings.operator)) {
    }
    if (operator.enabled) {
      if (userServers.value[operatorIndex].smpServers.filter { !it.deleted }.isNotEmpty()) {

      }
    }
  }
}

@Composable
fun ModalData.OperatorInfoView(index: Int, serverOperator: ServerOperator) {
  ColumnWithScrollBar {
    AppBarTitle(stringResource(MR.strings.operator_info_title))

    SectionView(generalGetString(MR.strings.operator_description)) {
      Text(serverOperator.info.description)
    }
    SectionView(generalGetString(MR.strings.operator_website)) {
      val website = serverOperator.info.website
      val uriHandler = LocalUriHandler.current
      Text(website, color = MaterialTheme.colors.primary, modifier = Modifier.clickable { uriHandler.openUriCatching(website) })
    }
  }
}