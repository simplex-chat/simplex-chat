package chat.simplex.common.views.usersettings.networkAndServers

import SectionDividerSpaced
import SectionItemView
import SectionTextFooter
import SectionView
import TextIconSpaced
import androidx.compose.foundation.*
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.alpha
import androidx.compose.ui.graphics.*
import androidx.compose.ui.platform.LocalUriHandler
import androidx.compose.ui.text.intl.Locale
import androidx.compose.ui.unit.dp
import chat.simplex.common.model.*
import chat.simplex.common.model.ChatController.getUsageConditions
import chat.simplex.common.platform.ColumnWithScrollBar
import chat.simplex.common.platform.chatModel
import chat.simplex.common.views.helpers.*
import chat.simplex.res.MR
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import kotlinx.coroutines.launch
import kotlinx.datetime.*
import java.time.format.DateTimeFormatter
import java.time.format.FormatStyle

@Composable
fun ModalData.OperatorView(currUserServers: MutableState<List<UserOperatorServers>>, userServers: MutableState<List<UserOperatorServers>>, operatorIndex: Int, rhId: Long?) {
  val testing = remember { mutableStateOf(false) }
  val operator = remember { userServers.value[operatorIndex].operator_ }

  ColumnWithScrollBar(Modifier.alpha(if (testing.value) 0.6f else 1f)) {
    AppBarTitle(String.format(stringResource(MR.strings.operator_servers_title), operator.tradeName))
    OperatorViewLayout(currUserServers, userServers, operatorIndex, rhId)
    if (testing.value) {
      DefaultProgressView(null)
    }
  }
}

@Composable
fun OperatorViewLayout(currUserServers: MutableState<List<UserOperatorServers>>, userServers: MutableState<List<UserOperatorServers>>, operatorIndex: Int, rhId: Long?) {
  val operator = remember { userServers.value[operatorIndex].operator_ }

  Column {
    SectionView(generalGetString(MR.strings.operator).uppercase()) {
      SectionItemView({ ModalManager.start.showModalCloseable { _ -> OperatorInfoView(operator) } }) {
        Image(
          painterResource(MR.images.decentralized),
          operator.tradeName,
          modifier = Modifier.size(24.dp),
          colorFilter = if (operator.enabled) null else ColorFilter.colorMatrix(ColorMatrix().apply {
            setToSaturation(0f)
          })
        )
        TextIconSpaced()
        Text(operator.tradeName, color = MaterialTheme.colors.onBackground)
      }
      UseOperatorToggle(currUserServers = currUserServers, userServers = userServers, operatorIndex = operatorIndex, rhId = rhId)
    }
    if (operator.enabled) {
      if (userServers.value[operatorIndex].smpServers.filter { !it.deleted }.isNotEmpty()) {

      }
    }
  }
}

@Composable
private fun OperatorInfoView(serverOperator: ServerOperator) {
  ColumnWithScrollBar {
    AppBarTitle(stringResource(MR.strings.operator_info_title))

    SectionView(generalGetString(MR.strings.operator_description).uppercase()) {
      SectionItemView {
        Text(serverOperator.info.description)
      }
    }
    SectionDividerSpaced()
    SectionView(generalGetString(MR.strings.operator_website).uppercase()) {
      SectionItemView {
        val website = serverOperator.info.website
        val uriHandler = LocalUriHandler.current
        Text(website, color = MaterialTheme.colors.primary, modifier = Modifier.clickable { uriHandler.openUriCatching(website) })
      }
    }
  }
}

@Composable
private fun UseOperatorToggle(
  currUserServers: MutableState<List<UserOperatorServers>>,
  userServers: MutableState<List<UserOperatorServers>>,
  operatorIndex: Int,
  rhId: Long?
) {
  val operator = remember { userServers.value[operatorIndex].operator_ }
  SectionItemView {
    Text(
      stringResource(MR.strings.operator_use_operator_toggle_description),
      Modifier.padding(end = 24.dp),
      color = Color.Unspecified
    )
    Spacer(Modifier.fillMaxWidth().weight(1f))
    DefaultSwitch(
      checked = operator.enabled,
      onCheckedChange = { enabled ->
        if (enabled) {
          when (operator.conditionsAcceptance) {
            is ConditionsAcceptance.Accepted -> {
              userServers.value[operatorIndex].operator_.enabled = true
            }

            is ConditionsAcceptance.Required -> {
              if (operator.conditionsAcceptance.deadline == null) {
                val deadline = operator.conditionsAcceptance.deadline
                // show conditions modal

                ModalManager.start.showModalCloseable { _ ->
                  SingleOperatorUsageConditionsView(
                    currUserServers = currUserServers,
                    userServers = userServers,
                    operatorIndex = operatorIndex,
                    rhId = rhId
                  )
                }
              } else {
                userServers.value[operatorIndex].operator_.enabled = true
              }
            }
          }
        } else {
          userServers.value[operatorIndex].operator_.enabled = false
        }
      },
    )
  }
}

@Composable
private fun SingleOperatorUsageConditionsView(
  currUserServers: MutableState<List<UserOperatorServers>>,
  userServers: MutableState<List<UserOperatorServers>>,
  operatorIndex: Int,
  rhId: Long?
) {
  val operatorsWithConditionsAccepted = remember { chatModel.conditions.value.serverOperators.filter { it.conditionsAcceptance.conditionsAccepted } }
  val operator = remember { userServers.value[operatorIndex].operator_ }

  ColumnWithScrollBar {
    AppBarTitle(String.format(stringResource(MR.strings.use_operator_x), operator.tradeName))

    if (operator.conditionsAcceptance is ConditionsAcceptance.Accepted) {
      // In current UI implementation this branch doesn't get shown - as conditions can't be opened from inside operator once accepted
      ConditionsTextView(rhId)
    } else if (operatorsWithConditionsAccepted.isNotEmpty()) {
      //
    } else {
      //
    }
  }
}

@Composable
private fun ConditionsTextView(
  rhId: Long?
) {
  val conditionsData = remember { mutableStateOf<Triple<UsageConditionsDetail, String?, CR.UsageConditions?>?>(null) }
  val failedToLoad = remember { mutableStateOf(false) }
  val defaultConditionsLink = "https://github.com/simplex-chat/simplex-chat/blob/stable/PRIVACY.md"
  val scope = rememberCoroutineScope()

  LaunchedEffect(Unit) {
    scope.launch {
      try {
        val conditions = getUsageConditions(rh = rhId)

        if (conditions != null) {
          conditionsData.value = conditions
        } else {
          failedToLoad.value = true
        }
      } catch (ex: Exception) {
        failedToLoad.value = true
      }
    }
  }
  val conditions = conditionsData.value

  if (conditions != null) {
    val (usageConditions, conditionsText, _) = conditions

    if (conditionsText != null) {
      Box(
        modifier = Modifier
          .background(
            color = MaterialTheme.colors.secondaryVariant,
            shape = RoundedCornerShape(12.dp)
          )
          .padding(8.dp)
      ) {
        ColumnWithScrollBar {
          Text(
            text = conditionsText.trim(),
            modifier = Modifier.padding(16.dp)
          )
        }
      }
    } else {
      val conditionsLink = "https://github.com/simplex-chat/simplex-chat/blob/${usageConditions.conditionsCommit}/PRIVACY.md"
      ConditionsLinkView(conditionsLink)
    }
  } else if (failedToLoad.value) {
    ConditionsLinkView(defaultConditionsLink)
  } else {
    DefaultProgressView(null)
  }
}

@Composable
private fun ConditionsLinkView(conditionsLink: String) {

  SectionItemView {
    val uriHandler = LocalUriHandler.current
    Text(stringResource(MR.strings.operator_conditions_failed_to_load), color = MaterialTheme.colors.onBackground)
    Text(conditionsLink, color = MaterialTheme.colors.primary, modifier = Modifier.clickable { uriHandler.openUriCatching(conditionsLink) })
  }
}