package chat.simplex.common.views.helpers

import InfoRow
import InfoRowTwoValues
import SectionBottomSpacer
import SectionItemViewSpaceBetween
import SectionTextFooter
import SectionView
import androidx.compose.foundation.clickable
import androidx.compose.foundation.layout.Arrangement
import androidx.compose.foundation.layout.Box
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.PaddingValues
import androidx.compose.foundation.layout.Row
import androidx.compose.foundation.layout.Spacer
import androidx.compose.foundation.layout.fillMaxSize
import androidx.compose.foundation.layout.fillMaxWidth
import androidx.compose.foundation.layout.height
import androidx.compose.foundation.layout.padding
import androidx.compose.foundation.pager.HorizontalPager
import androidx.compose.foundation.pager.rememberPagerState
import androidx.compose.foundation.text.selection.SelectionContainer
import androidx.compose.material.Divider
import androidx.compose.material.Icon
import androidx.compose.material.LeadingIconTab
import androidx.compose.material.MaterialTheme
import androidx.compose.material.TabRow
import androidx.compose.material.Text
import androidx.compose.runtime.Composable
import androidx.compose.runtime.DisposableEffect
import androidx.compose.runtime.LaunchedEffect
import androidx.compose.runtime.getValue
import androidx.compose.runtime.mutableStateOf
import androidx.compose.runtime.remember
import androidx.compose.runtime.rememberCoroutineScope
import androidx.compose.runtime.setValue
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.platform.LocalClipboardManager
import androidx.compose.ui.text.TextStyle
import androidx.compose.ui.text.font.FontFamily
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import chat.simplex.common.model.AgentSMPServerStatsData
import chat.simplex.common.model.AgentXFTPServerStatsData
import chat.simplex.common.model.ChatController.chatModel
import chat.simplex.common.model.ChatModel.controller
import chat.simplex.common.model.OnionHosts
import chat.simplex.common.model.PresentedServersSummary
import chat.simplex.common.model.RemoteHostInfo
import chat.simplex.common.model.SMPServerSubs
import chat.simplex.common.model.SMPServerSummary
import chat.simplex.common.model.SMPTotals
import chat.simplex.common.model.ServerAddress.Companion.parseServerAddress
import chat.simplex.common.model.ServerProtocol
import chat.simplex.common.model.ServerSessions
import chat.simplex.common.model.XFTPServerSummary
import chat.simplex.common.model.localTimestamp
import chat.simplex.common.platform.ColumnWithScrollBar
import chat.simplex.common.platform.shareText
import chat.simplex.common.ui.theme.DEFAULT_PADDING
import chat.simplex.common.ui.theme.DEFAULT_PADDING_HALF
import chat.simplex.common.views.usersettings.ProtocolServersView
import chat.simplex.res.MR
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import kotlinx.coroutines.Job
import kotlinx.coroutines.cancel
import kotlinx.coroutines.delay
import kotlinx.coroutines.launch
import kotlinx.datetime.Instant
import numOrDash
import java.text.DecimalFormat
import kotlin.math.floor
import kotlin.math.roundToInt
import kotlin.time.Duration
import kotlin.time.Duration.Companion.seconds

enum class SubscriptionColorType {
  ACTIVE, ONION_ACTIVE, DISCONNECTED, ACTIVE_DISCONNECTED
}

data class SubscriptionStatus(
  val color: SubscriptionColorType,
  val variableValue: Float,
  val opacity: Float,
  val statusPercent: Float
)

fun subscriptionStatusColorAndPercentage(
  online: Boolean,
  onionHosts: OnionHosts,
  subs: SMPServerSubs,
  sess: ServerSessions
): SubscriptionStatus {

  fun roundedToQuarter(n: Float): Float {
    return when {
      n >= 1 -> 1f
      n <= 0 -> 0f
      else -> (n * 4).roundToInt() / 4f
    }
  }

  val activeColor: SubscriptionColorType = if (onionHosts == OnionHosts.REQUIRED) SubscriptionColorType.ONION_ACTIVE else SubscriptionColorType.ACTIVE
  val noConnColorAndPercent = SubscriptionStatus(SubscriptionColorType.DISCONNECTED, 1f, 1f, 0f)
  val activeSubsRounded = roundedToQuarter(subs.shareOfActive)

  return if (online && subs.total > 0) {
    if (subs.ssActive == 0) {
      if (sess.ssConnected == 0) noConnColorAndPercent else SubscriptionStatus(activeColor, activeSubsRounded, subs.shareOfActive, subs.shareOfActive)
    } else {
      if (sess.ssConnected == 0) SubscriptionStatus(SubscriptionColorType.ACTIVE_DISCONNECTED, activeSubsRounded, subs.shareOfActive, subs.shareOfActive)
      else SubscriptionStatus(activeColor, activeSubsRounded, subs.shareOfActive, subs.shareOfActive)
    }
  } else noConnColorAndPercent
}

@Composable
private fun SectionDivider() {
  Divider(
    Modifier.padding(
      start = DEFAULT_PADDING_HALF,
      top = 32.dp,
      end = DEFAULT_PADDING_HALF,
      bottom = 30.dp
    )
  )
}

@Composable
fun SubscriptionStatusIndicatorView(subs: SMPServerSubs, sess: ServerSessions, leadingPercentage: Boolean = false) {
  val netCfg = remember { chatModel.controller.getNetCfg() }
  val onionHosts = remember { netCfg.onionHosts }
  val statusColorAndPercentage = subscriptionStatusColorAndPercentage(chatModel.networkInfo.value.online, onionHosts, subs, sess)
  val pref = remember { chatModel.controller.appPrefs.networkShowSubscriptionPercentage }
  val percentageText = "${(floor(statusColorAndPercentage.statusPercent * 100)).toInt()}%"

  Row(verticalAlignment = Alignment.CenterVertically, horizontalArrangement = Arrangement.spacedBy(4.dp)) {
    if (pref.state.value && leadingPercentage) Text(percentageText, color = MaterialTheme.colors.secondary)
    SubscriptionStatusIcon(
      color = when(statusColorAndPercentage.color) {
        SubscriptionColorType.ACTIVE -> MaterialTheme.colors.primary
        SubscriptionColorType.ONION_ACTIVE -> MaterialTheme.colors.primaryVariant
        SubscriptionColorType.ACTIVE_DISCONNECTED -> MaterialTheme.colors.onBackground
        SubscriptionColorType.DISCONNECTED -> MaterialTheme.colors.error
      },
      variableValue = statusColorAndPercentage.variableValue)
    if (pref.state.value && !leadingPercentage) Text(percentageText, color = MaterialTheme.colors.secondary)
  }
}

@Composable
fun SubscriptionStatusIndicator(click: ((PresentedServersSummary?) -> Unit)? = null, ) {
  var subs by remember { mutableStateOf(SMPServerSubs.newSMPServerSubs) }
  var sess by remember { mutableStateOf(ServerSessions.newServerSessions) }
  var timerCounter by remember { mutableStateOf(0) }
  var timer: Job? by remember { mutableStateOf(null) }
  var summary: PresentedServersSummary? by remember { mutableStateOf(null) }

  val initialInterval: Duration = 1.seconds
  val regularInterval: Duration = 3.seconds
  val initialPhaseDuration: Duration = 10.seconds

  val scope = rememberCoroutineScope()

  fun setServersSummary() {
    withBGApi {
      summary = chatModel.controller.getAgentServersSummary(chatModel.remoteHostId())

      summary?.let {
        subs = it.allUsersSMP.smpTotals.subs
        sess = it.allUsersSMP.smpTotals.sessions
      }
    }
  }

  fun stopTimer() {
    timer?.cancel()
    timer = null
  }

  fun switchToRegularTimer() {
    stopTimer()
    timer = timer ?: scope.launch {
      while (true) {
        delay(regularInterval.inWholeMilliseconds)
        setServersSummary()
      }
    }
  }

  fun startInitialTimer() {
    timer = timer ?: scope.launch {
      while (true) {
        delay(initialInterval.inWholeMilliseconds)
        setServersSummary()
        timerCounter++
        if (timerCounter *  initialInterval.inWholeSeconds >= initialPhaseDuration.inWholeSeconds) {
          switchToRegularTimer()
        }
      }
    }
  }

  DisposableEffect(Unit) {
    onDispose {
      stopTimer()
      scope.cancel()
    }
  }

  LaunchedEffect(Unit) {
    startInitialTimer()
  }

  Row(if (click != null) Modifier.clickable(onClick = { click(summary) }) else Modifier) {
    SubscriptionStatusIndicatorView(subs = subs, sess = sess)
  }
}

enum class PresentedUserCategory {
  CURRENT_USER, ALL_USERS
}

enum class PresentedServerType {
  SMP, XFTP
}

@Composable
private fun ServerSessionsView(sess: ServerSessions) {
  SectionView(generalGetString(MR.strings.servers_info_transport_sessions_section_header).uppercase()) {
    InfoRow(
      generalGetString(MR.strings.number_of_servers_connected),
      numOrDash(sess.ssConnected)
    )
    InfoRow(
      generalGetString(MR.strings.number_of_servers_with_connection_errors),
      numOrDash(sess.ssErrors)
    )
    InfoRow(
      generalGetString(MR.strings.number_of_servers_connecting),
      numOrDash(sess.ssConnecting)
    )
  }
}

private fun serverAddress(server: String): String {
  val address =  parseServerAddress(server)

  return address?.hostnames?.first() ?: server
}

@Composable
private fun SmpServerView(srvSumm: SMPServerSummary, statsStartedAt: Instant, rh: RemoteHostInfo?) {
  SectionItemViewSpaceBetween(
    click = {
      ModalManager.start.showCustomModal { close -> SMPServerSummaryView(
        rh = rh,
        close = close,
        summary = srvSumm,
        statsStartedAt = statsStartedAt)
      }
    }
  ) {
    Column(
      modifier = Modifier.fillMaxWidth(),
      verticalArrangement = Arrangement.Center,
    ) {
      Row(
        modifier = Modifier.fillMaxWidth(),
        verticalAlignment = Alignment.CenterVertically,
        horizontalArrangement = Arrangement.SpaceBetween
      ) {
        Text(serverAddress(srvSumm.smpServer))
        Row(horizontalArrangement = Arrangement.spacedBy(6.dp)) {
          if (srvSumm.subs != null && srvSumm.sessions != null) {
            SubscriptionStatusIndicatorView(subs = srvSumm.subs, sess = srvSumm.sessions, leadingPercentage = true)
          }
          RowLinkIcon("see server details")
        }
      }
    }
  }
}

@Composable
private fun SmpServersListView(servers: List<SMPServerSummary>, statsStartedAt: Instant, header: String? = null, footer: String? = null, rh: RemoteHostInfo?) {
  val sortedServers = servers.sortedWith(compareBy<SMPServerSummary> { !it.hasSubs }
    .thenBy { serverAddress(it.smpServer) })

  SectionView(header) {
    sortedServers.map { svr -> SmpServerView(srvSumm = svr, statsStartedAt = statsStartedAt, rh = rh)}
  }
  if (footer != null) {
    SectionTextFooter(
      footer
    )
  }
}

fun prettySize(sizeInKB: Long): String {
  if (sizeInKB == 0L) {
    return "-"
  }

  val sizeInBytes = sizeInKB * 1024
  val units = arrayOf("B", "KB", "MB", "GB", "TB", "PB", "EB", "ZB", "YB")
  var size = sizeInBytes.toDouble()
  var unitIndex = 0

  while (size >= 1024 && unitIndex < units.size - 1) {
    size /= 1024
    unitIndex++
  }

  val formatter = DecimalFormat("#,##0.#")
  return "${formatter.format(size)} ${units[unitIndex]}"
}

@Composable
private fun inProgressIcon(srvSumm: XFTPServerSummary): Unit? {
  return when {
    !srvSumm.rcvInProgress && !srvSumm.sndInProgress && !srvSumm.delInProgress -> null
    srvSumm.rcvInProgress && !srvSumm.sndInProgress && !srvSumm.delInProgress -> Icon(painterResource(MR.images.ic_arrow_downward),"download", tint = MaterialTheme.colors.secondary)
    !srvSumm.rcvInProgress && srvSumm.sndInProgress && !srvSumm.delInProgress -> Icon(painterResource(MR.images.ic_arrow_upward), "upload", tint = MaterialTheme.colors.secondary)
    !srvSumm.rcvInProgress && !srvSumm.sndInProgress && srvSumm.delInProgress -> Icon(painterResource(MR.images.ic_delete), "deleted", tint = MaterialTheme.colors.secondary)
    else -> Icon(painterResource(MR.images.ic_expand_all), "upload and download", tint = MaterialTheme.colors.secondary)
  }
}

@Composable
private fun XftpServerView(srvSumm: XFTPServerSummary, statsStartedAt: Instant, rh: RemoteHostInfo?) {
  SectionItemViewSpaceBetween(
    click = {
      ModalManager.start.showCustomModal { close -> XFTPServerSummaryView(
        rh = rh,
        close = close,
        summary = srvSumm,
        statsStartedAt = statsStartedAt)
      }
    }
  ) {
    Column(
      modifier = Modifier.fillMaxWidth(),
      verticalArrangement = Arrangement.Center,
    ) {
      Row(
        modifier = Modifier.fillMaxWidth(),
        verticalAlignment = Alignment.CenterVertically,
        horizontalArrangement = Arrangement.SpaceBetween
      ) {
        Text(serverAddress(srvSumm.xftpServer))
        Row(horizontalArrangement = Arrangement.spacedBy(6.dp)) {
          inProgressIcon(srvSumm)
          RowLinkIcon("see server details")
        }
      }
    }
  }
}

@Composable
private fun XftpServersListView(servers: List<XFTPServerSummary>, statsStartedAt: Instant, header: String? = null, rh: RemoteHostInfo?) {
  val sortedServers = servers.sortedBy { serverAddress(it.xftpServer) }

  SectionView(header) {
    sortedServers.map { svr -> XftpServerView(svr, statsStartedAt, rh)}
  }
}

@Composable
private fun RowLinkIcon(contentDescription: String) {
  return Icon(painterResource(MR.images.ic_chevron_right), contentDescription, tint = MaterialTheme.colors.secondary)
}

@Composable
private fun SMPStatsView(stats: AgentSMPServerStatsData, statsStartedAt: Instant, remoteHostInfo: RemoteHostInfo?) {
  SectionView(generalGetString(MR.strings.servers_info_statistics_section_header).uppercase()) {
    InfoRow(
      generalGetString(MR.strings.servers_info_messages_sent),
      numOrDash(stats._sentDirect + stats._sentViaProxy)
    )
    InfoRow(
      generalGetString(MR.strings.servers_info_messages_received),
      numOrDash(stats._recvMsgs)
    )
    SectionItemViewSpaceBetween(
      click = {
        ModalManager.start.showCustomModal { close -> DetailedSMPStatsView(
          rh = remoteHostInfo,
          close = close,
          stats = stats,
          statsStartedAt = statsStartedAt)
        }
      }
    ) {
      Row(
        verticalAlignment = Alignment.CenterVertically,
        horizontalArrangement = Arrangement.SpaceBetween,
        modifier = Modifier.fillMaxWidth(),
        ) {
        Text(text = generalGetString(MR.strings.servers_info_details), color = MaterialTheme.colors.onBackground)
        RowLinkIcon("see details")
      }
    }
  }
  SectionTextFooter(
    String.format(stringResource(MR.strings.servers_info_private_data_disclaimer), localTimestamp(statsStartedAt))
  )
}

@Composable
private fun SMPSubscriptionsSection(totals: SMPTotals) {
  Column {
    Row(Modifier.padding(start = DEFAULT_PADDING, bottom = 5.dp), verticalAlignment = Alignment.CenterVertically, horizontalArrangement = Arrangement.spacedBy(6.dp)) {
      Text(generalGetString(MR.strings.servers_info_subscriptions_section_header).uppercase(), color = MaterialTheme.colors.secondary, style = MaterialTheme.typography.body2, fontSize = 12.sp)
      SubscriptionStatusIndicatorView(totals.subs, totals.sessions)
    }
    Column(Modifier.padding(PaddingValues()).fillMaxWidth()) {
      InfoRow(
        generalGetString(MR.strings.servers_info_subscriptions_connections_subscribed),
        numOrDash(totals.subs.ssActive)
      )
      InfoRow(
        generalGetString(MR.strings.servers_info_subscriptions_total),
        numOrDash(totals.subs.total)
      )
    }
  }
}

@Composable
private fun SMPSubscriptionsSection(subs: SMPServerSubs, summary: SMPServerSummary, rh: RemoteHostInfo?) {
  Column {
    Row(Modifier.padding(start = DEFAULT_PADDING, bottom = 5.dp), verticalAlignment = Alignment.CenterVertically, horizontalArrangement = Arrangement.spacedBy(6.dp)) {
      Text(generalGetString(MR.strings.servers_info_subscriptions_section_header).uppercase(), color = MaterialTheme.colors.secondary, style = MaterialTheme.typography.body2, fontSize = 12.sp)
      SubscriptionStatusIndicatorView(subs, summary.sessionsOrNew)
    }
    Column(Modifier.padding(PaddingValues()).fillMaxWidth()) {
      InfoRow(
        generalGetString(MR.strings.servers_info_subscriptions_connections_subscribed),
        numOrDash(subs.ssActive)
      )
      InfoRow(
        generalGetString(MR.strings.servers_info_subscriptions_connections_pending),
        numOrDash(subs.ssPending)
      )
      InfoRow(
        generalGetString(MR.strings.servers_info_subscriptions_total),
        numOrDash(subs.total)
      )
      SectionItemViewSpaceBetween {
        Row {
          Text(
            stringResource(MR.strings.reconnect),
            modifier = Modifier.clickable() {
              AlertManager.shared.showAlertDialog(
                title = generalGetString(MR.strings.servers_info_reconnect_server_title),
                text = generalGetString(MR.strings.servers_info_reconnect_server_message),
                dismissText = generalGetString(MR.strings.servers_info_modal_dismiss),
                destructive = true,
                onConfirm = {
                  withBGApi {
                    val success = controller.reconnectServer(rh?.remoteHostId, summary.smpServer)

                    if (!success) {
                      AlertManager.shared.showAlertMsg(
                        title = generalGetString(MR.strings.servers_info_modal_error_title),
                        text = generalGetString(MR.strings.servers_info_reconnect_server_error)
                      )
                    }
                  }
                }
              )
            },
            color = MaterialTheme.colors.primary
          )
        }
      }
    }
  }
}

@Composable
fun XFTPStatsView(stats: AgentXFTPServerStatsData, statsStartedAt: Instant, rh: RemoteHostInfo?) {
  SectionView(generalGetString(MR.strings.servers_info_statistics_section_header).uppercase()) {
    InfoRow(
      generalGetString(MR.strings.servers_info_uploaded),
      prettySize(stats._uploadsSize)
    )
    InfoRow(
      generalGetString(MR.strings.servers_info_downloaded),
      prettySize(stats._downloadsSize)
    )
    SectionItemViewSpaceBetween(
      click = {
        ModalManager.start.showCustomModal { close -> DetailedXFTPStatsView(
          rh = rh,
          close = close,
          stats = stats,
          statsStartedAt = statsStartedAt)
        }
      }
    ) {
      Row(
        verticalAlignment = Alignment.CenterVertically,
        horizontalArrangement = Arrangement.SpaceBetween,
        modifier = Modifier.fillMaxWidth(),
      ) {
        Text(text = generalGetString(MR.strings.servers_info_details), color = MaterialTheme.colors.onBackground)
        RowLinkIcon("see details")
      }
    }
  }
  SectionTextFooter(
    String.format(stringResource(MR.strings.servers_info_private_data_disclaimer), localTimestamp(statsStartedAt))
  )
}

@Composable
private fun IndentedInfoRow(title: String, desc: String) {
  InfoRow(title, desc, padding = PaddingValues(
    start = 24.dp + DEFAULT_PADDING,
    end = DEFAULT_PADDING,
    bottom = DEFAULT_PADDING,
    top = DEFAULT_PADDING)
  )
}

@Composable
fun DetailedSMPStatsLayout(stats: AgentSMPServerStatsData, statsStartedAt: Instant) {
  SectionView(generalGetString(MR.strings.servers_info_detailed_statistics_sent_messages_header).uppercase()) {
    InfoRow(generalGetString(MR.strings.servers_info_detailed_statistics_sent_messages_total), numOrDash(stats._sentDirect + stats._sentViaProxy))
    InfoRowTwoValues(generalGetString(MR.strings.sent_directly), generalGetString(MR.strings.attempts_label), stats._sentDirect, stats._sentDirectAttempts)
    InfoRowTwoValues(generalGetString(MR.strings.sent_via_proxy), generalGetString(MR.strings.attempts_label), stats._sentViaProxy, stats._sentViaProxyAttempts)
    InfoRowTwoValues(generalGetString(MR.strings.proxied), generalGetString(MR.strings.attempts_label), stats._sentProxied, stats._sentProxiedAttempts)
    SectionItemViewSpaceBetween {
      Row {
        Text(generalGetString(MR.strings.send_errors), color = MaterialTheme.colors.onBackground)
      }
    }
    IndentedInfoRow(generalGetString(MR.strings.auth), numOrDash(stats._sentAuthErrs))
    IndentedInfoRow(generalGetString(MR.strings.quota), numOrDash(stats._sentQuotaErrs))
    IndentedInfoRow(generalGetString(MR.strings.expired_label), numOrDash(stats._sentExpiredErrs))
    IndentedInfoRow(generalGetString(MR.strings.other_label), numOrDash(stats._sentOtherErrs))
  }
  SectionDivider()
  SectionView(generalGetString(MR.strings.servers_info_detailed_statistics_received_messages_header).uppercase()) {
    InfoRow(generalGetString(MR.strings.servers_info_detailed_statistics_received_total), numOrDash(stats._recvMsgs))
    SectionItemViewSpaceBetween {
      Row {
        Text(generalGetString(MR.strings.servers_info_detailed_statistics_receive_errors), color = MaterialTheme.colors.onBackground)
      }
    }
    IndentedInfoRow(generalGetString(MR.strings.duplicates_label), numOrDash(stats._recvDuplicates))
    IndentedInfoRow(generalGetString(MR.strings.decryption_errors), numOrDash(stats._recvCryptoErrs))
    IndentedInfoRow(generalGetString(MR.strings.other_errors), numOrDash(stats._recvErrs))
    InfoRowTwoValues(generalGetString(MR.strings.acknowledged), generalGetString(MR.strings.attempts_label), stats._ackMsgs, stats._ackAttempts)
    SectionItemViewSpaceBetween {
      Row {
        Text(generalGetString(MR.strings.acknowledgement_errors), color = MaterialTheme.colors.onBackground)
      }
    }
    IndentedInfoRow(generalGetString(MR.strings.no_msg_errors), numOrDash(stats._ackNoMsgErrs))
    IndentedInfoRow(generalGetString(MR.strings.other_errors), numOrDash(stats._ackOtherErrs))
  }
  SectionDivider()
  SectionView(generalGetString(MR.strings.connections).uppercase()) {
    InfoRow(generalGetString(MR.strings.created), numOrDash(stats._connCreated))
    InfoRow(generalGetString(MR.strings.secured), numOrDash(stats._connCreated))
    InfoRow(generalGetString(MR.strings.completed), numOrDash(stats._connCompleted))
    InfoRowTwoValues(generalGetString(MR.strings.deleted), generalGetString(MR.strings.attempts_label), stats._connDeleted, stats._connDelAttempts)
    InfoRow(generalGetString(MR.strings.deletion_errors), numOrDash(stats._connDelErrs))
    InfoRowTwoValues(generalGetString(MR.strings.subscribed), generalGetString(MR.strings.attempts_label), stats._connSubscribed, stats._connSubAttempts)
    InfoRow(generalGetString(MR.strings.subscription_results_ignored), numOrDash(stats._connSubIgnored))
    InfoRow(generalGetString(MR.strings.subscription_errors), numOrDash(stats._connSubErrs))
  }
  SectionTextFooter(
    String.format(stringResource(MR.strings.servers_info_starting_from), localTimestamp(statsStartedAt))
  )

  SectionBottomSpacer()
}

@Composable
fun DetailedXFTPStatsLayout(stats: AgentXFTPServerStatsData, statsStartedAt: Instant) {
  SectionView(generalGetString(MR.strings.uploaded_files).uppercase()) {
    InfoRow(generalGetString(MR.strings.size), prettySize(stats._uploadsSize))
    InfoRowTwoValues(generalGetString(MR.strings.chunks_uploaded), generalGetString(MR.strings.attempts_label), stats._uploads, stats._uploadAttempts)
    InfoRow(generalGetString(MR.strings.upload_errors), numOrDash(stats._uploadErrs))
    InfoRowTwoValues(generalGetString(MR.strings.chunks_deleted), generalGetString(MR.strings.attempts_label), stats._deletions, stats._deleteAttempts)
    InfoRow(generalGetString(MR.strings.deletion_errors), numOrDash(stats._deleteErrs))
  }
  SectionDivider()
  SectionView(generalGetString(MR.strings.downloaded_files).uppercase()) {
    InfoRow(generalGetString(MR.strings.size), prettySize(stats._downloadsSize))
    InfoRowTwoValues(generalGetString(MR.strings.chunks_downloaded), generalGetString(MR.strings.attempts_label), stats._downloads, stats._downloadAttempts)
    SectionItemViewSpaceBetween {
      Row {
        Text(generalGetString(MR.strings.download_errors), color = MaterialTheme.colors.onBackground)
      }
    }
    IndentedInfoRow(generalGetString(MR.strings.auth), numOrDash(stats._downloadAuthErrs))
    IndentedInfoRow(generalGetString(MR.strings.other_label), numOrDash(stats._downloadErrs))
  }
  SectionTextFooter(
    String.format(stringResource(MR.strings.servers_info_starting_from), localTimestamp(statsStartedAt))
  )

  SectionBottomSpacer()
}

@Composable
fun XFTPServerSummaryLayout(summary: XFTPServerSummary, statsStartedAt: Instant, rh: RemoteHostInfo?) {
  SectionView(generalGetString(MR.strings.server_address).uppercase()) {
    SelectionContainer {
      Text(
        summary.xftpServer,
        Modifier.padding(start = DEFAULT_PADDING, top = 5.dp, end = DEFAULT_PADDING, bottom = 10.dp),
        style = TextStyle(
          fontFamily = FontFamily.Monospace, fontSize = 16.sp,
          color = MaterialTheme.colors.secondary
        )
      )
    }
    if (summary.known == true) {
      Text(
        generalGetString(MR.strings.open_server_settings_button),
        modifier = Modifier.padding(DEFAULT_PADDING).clickable() {
          ModalManager.start.showCustomModal { close -> ProtocolServersView(chatModel, rhId = rh?.remoteHostId, ServerProtocol.XFTP, close) }
        },
        color = MaterialTheme.colors.primary
      )
    }

    if (summary.stats != null) {
      SectionDivider()
      XFTPStatsView(stats = summary.stats, rh = rh, statsStartedAt = statsStartedAt)
    }

    if (summary.sessions != null) {
      SectionDivider()
      ServerSessionsView(summary.sessions)
    }
  }

  SectionBottomSpacer()
}

@Composable
fun SMPServerSummaryLayout(summary: SMPServerSummary, statsStartedAt: Instant, rh: RemoteHostInfo?) {
  SectionView(generalGetString(MR.strings.server_address).uppercase()) {
    SelectionContainer {
      Text(
        summary.smpServer,
        Modifier.padding(start = DEFAULT_PADDING, top = 5.dp, end = DEFAULT_PADDING, bottom = 10.dp),
        style = TextStyle(
          fontFamily = FontFamily.Monospace, fontSize = 16.sp,
          color = MaterialTheme.colors.secondary
        )
      )
    }
    if (summary.known == true) {
      Text(
        generalGetString(MR.strings.open_server_settings_button),
        modifier = Modifier.padding(DEFAULT_PADDING).clickable() {
          ModalManager.start.showCustomModal { close -> ProtocolServersView(chatModel, rhId = rh?.remoteHostId, ServerProtocol.SMP, close) }
        },
        color = MaterialTheme.colors.primary
      )
    }

    if (summary.stats != null) {
      SectionDivider()
      SMPStatsView(stats = summary.stats, remoteHostInfo = rh, statsStartedAt = statsStartedAt)
    }

    if (summary.subs != null) {
      SectionDivider()
      SMPSubscriptionsSection(subs = summary.subs, summary = summary, rh = rh)
    }

    if (summary.sessions != null) {
      SectionDivider()
      ServerSessionsView(summary.sessions)
    }
  }

  SectionBottomSpacer()
}

@Composable
fun ModalData.SMPServerSummaryView(
  rh: RemoteHostInfo?,
  close: () -> Unit,
  summary: SMPServerSummary,
  statsStartedAt: Instant
) {
  ModalView(
    close = {
      close()
    }
  ) {
    ColumnWithScrollBar(
      Modifier.fillMaxSize(),
    ) {
      Box(contentAlignment = Alignment.Center) {
        val bottomPadding = DEFAULT_PADDING
        AppBarTitle(
          stringResource(MR.strings.smp_server),
          hostDevice(rh?.remoteHostId),
          bottomPadding = bottomPadding
        )
      }
      SMPServerSummaryLayout(summary, statsStartedAt, rh)
    }
  }
}


@Composable
fun ModalData.DetailedXFTPStatsView(
  rh: RemoteHostInfo?,
  close: () -> Unit,
  stats: AgentXFTPServerStatsData,
  statsStartedAt: Instant
) {
  ModalView(
    close = {
      close()
    }
  ) {
    ColumnWithScrollBar(
      Modifier.fillMaxSize(),
    ) {
      Box(contentAlignment = Alignment.Center) {
        val bottomPadding = DEFAULT_PADDING
        AppBarTitle(
          stringResource(MR.strings.servers_info_detailed_statistics),
          hostDevice(rh?.remoteHostId),
          bottomPadding = bottomPadding
        )
      }
      DetailedXFTPStatsLayout(stats, statsStartedAt)
    }
  }
}

@Composable
fun ModalData.DetailedSMPStatsView(
  rh: RemoteHostInfo?,
  close: () -> Unit,
  stats: AgentSMPServerStatsData,
  statsStartedAt: Instant
) {
  ModalView(
    close = {
      close()
    }
  ) {
    ColumnWithScrollBar(
      Modifier.fillMaxSize(),
    ) {
      Box(contentAlignment = Alignment.Center) {
        val bottomPadding = DEFAULT_PADDING
        AppBarTitle(
          stringResource(MR.strings.servers_info_detailed_statistics),
          hostDevice(rh?.remoteHostId),
          bottomPadding = bottomPadding
        )
      }
      DetailedSMPStatsLayout(stats, statsStartedAt)
    }
  }
}

@Composable
fun ModalData.XFTPServerSummaryView(
  rh: RemoteHostInfo?,
  close: () -> Unit,
  summary: XFTPServerSummary,
  statsStartedAt: Instant
) {
  ModalView(
    close = {
      close()
    }
  ) {
    ColumnWithScrollBar(
      Modifier.fillMaxSize(),
    ) {
      Box(contentAlignment = Alignment.Center) {
        val bottomPadding = DEFAULT_PADDING
        AppBarTitle(
          stringResource(MR.strings.xftp_server),
          hostDevice(rh?.remoteHostId),
          bottomPadding = bottomPadding
        )
      }
      XFTPServerSummaryLayout(summary, statsStartedAt, rh)
    }
  }
}

@Composable
fun ModalData.ServersSummaryView(rh: RemoteHostInfo?) {
  Column(
    Modifier.fillMaxSize(),
  ) {
    var timer: Job? by remember { mutableStateOf(null) }
    var showUserSelection by remember { mutableStateOf(false) }
    var serversSummary by remember { mutableStateOf<PresentedServersSummary?>(null) }
    val selectedUserCategory =
      remember { stateGetOrPut("selectedUserCategory") { PresentedUserCategory.ALL_USERS } }
    val selectedServerType =
      remember { stateGetOrPut("serverTypeSelection") { PresentedServerType.SMP } }
    val scope = rememberCoroutineScope()
    val fetchInterval: Duration = 1.seconds

    fun getServersSummary() {
      withBGApi {
        serversSummary = chatModel.controller.getAgentServersSummary(chatModel.remoteHostId())
      }
    }

    LaunchedEffect(Unit) {
      if (chatModel.users.count { u -> u.user.activeUser || !u.user.hidden } == 1
      ) {
        selectedUserCategory.value = PresentedUserCategory.CURRENT_USER
      } else {
        showUserSelection = true
      }
      getServersSummary()
      timer = timer ?: scope.launch {
        while (true) {
          delay(fetchInterval.inWholeMilliseconds)
          getServersSummary()
        }
      }
    }

    DisposableEffect(Unit) {
      onDispose {
        timer?.cancel()
        timer = null
        scope.cancel()
      }
    }

    Column(
      Modifier.fillMaxSize(),
    ) {
      Box(contentAlignment = Alignment.Center) {
        val bottomPadding = DEFAULT_PADDING
        AppBarTitle(
          stringResource(MR.strings.servers_info),
          hostDevice(rh?.remoteHostId),
          bottomPadding = bottomPadding
        )
      }
      if (serversSummary == null) {
        return Text(generalGetString(MR.strings.servers_info_missing))
      }

      val userOptions by remember {
        mutableStateOf(
          listOf(
            PresentedUserCategory.ALL_USERS to generalGetString(MR.strings.all_users),
            PresentedUserCategory.CURRENT_USER to generalGetString(MR.strings.current_user),
          )
        )
      }

      val serverTypeTabTitles = PresentedServerType.entries.map {
        when (it) {
          PresentedServerType.SMP ->
            stringResource(MR.strings.messages_section_title)

          PresentedServerType.XFTP ->
            stringResource(MR.strings.files_and_media_section)
        }
      }

      val serverTypePagerState = rememberPagerState(
        initialPage = selectedServerType.value.ordinal,
        initialPageOffsetFraction = 0f
      ) { PresentedServerType.entries.size }

      KeyChangeEffect(serverTypePagerState.currentPage) {
        selectedServerType.value = PresentedServerType.values()[serverTypePagerState.currentPage]
      }
      if (showUserSelection) {
        ExposedDropDownSettingRow(
          generalGetString(MR.strings.servers_info_target),
          userOptions,
          selectedUserCategory,
          icon = null,
          enabled = remember { mutableStateOf(true) },
          onSelected = {
            selectedUserCategory.value = it
          }
        )
      }
      TabRow(
        selectedTabIndex = serverTypePagerState.currentPage,
        backgroundColor = Color.Transparent,
        contentColor = MaterialTheme.colors.primary,
      ) {
        serverTypeTabTitles.forEachIndexed { index, it ->
          LeadingIconTab(
            selected = serverTypePagerState.currentPage == index,
            onClick = {
              scope.launch {
                serverTypePagerState.animateScrollToPage(index)
              }
            },
            text = { Text(it, fontSize = 13.sp) },
            icon = {
              Icon(
                if (PresentedServerType.SMP.ordinal == index) painterResource(MR.images.ic_mail) else painterResource(
                  MR.images.ic_download
                ),
                it
              )
            },
            selectedContentColor = MaterialTheme.colors.primary,
            unselectedContentColor = MaterialTheme.colors.secondary,
          )
        }
      }

      HorizontalPager(
        state = serverTypePagerState,
        Modifier.fillMaxSize(),
        verticalAlignment = Alignment.Top
      ) { index ->
        ColumnWithScrollBar(
          Modifier
            .fillMaxSize(),
          verticalArrangement = Arrangement.Top
        ) {
          Spacer(Modifier.height(DEFAULT_PADDING))
          when (index) {
            PresentedServerType.SMP.ordinal -> {
              val castedSummary = serversSummary!!
              val smpSummary =
                if (selectedUserCategory.value == PresentedUserCategory.CURRENT_USER) castedSummary.currentUserSMP else castedSummary.allUsersSMP;
              val totals = smpSummary.smpTotals
              val currentlyUsedSMPServers = smpSummary.currentlyUsedSMPServers
              val previouslyUsedSMPServers = smpSummary.previouslyUsedSMPServers
              val proxySMPServers = smpSummary.onlyProxiedSMPServers
              val statsStartedAt = castedSummary.statsStartedAt

              SMPStatsView(totals.stats, statsStartedAt, rh)
              SectionDivider()
              SMPSubscriptionsSection(totals)
              SectionDivider()

              if (currentlyUsedSMPServers.isNotEmpty()) {
                SmpServersListView(
                  servers = currentlyUsedSMPServers,
                  statsStartedAt = statsStartedAt,
                  header = generalGetString(MR.strings.servers_info_connected_servers_section_header).uppercase(),
                  rh = rh
                )
                SectionDivider()
              }

              if (previouslyUsedSMPServers.isNotEmpty()) {
                SmpServersListView(
                  servers = previouslyUsedSMPServers,
                  statsStartedAt = statsStartedAt,
                  header = generalGetString(MR.strings.servers_info_previously_connected_servers_section_header).uppercase(),
                  rh = rh
                )
                SectionDivider()
              }

              if (proxySMPServers.isNotEmpty()) {
                SmpServersListView(
                  servers = proxySMPServers,
                  statsStartedAt = statsStartedAt,
                  header = generalGetString(MR.strings.servers_info_proxied_servers_section_header).uppercase(),
                  footer = generalGetString(MR.strings.servers_info_proxied_servers_section_footer),
                  rh = rh
                )
                SectionDivider()
              }

              ServerSessionsView(totals.sessions)
            }

            PresentedServerType.XFTP.ordinal -> {
              val castedSummary = serversSummary!!
              val xftpSummary =
                if (selectedUserCategory.value == PresentedUserCategory.CURRENT_USER) castedSummary.currentUserXFTP else castedSummary.allUsersXFTP
              val totals = xftpSummary.xftpTotals
              val statsStartedAt = castedSummary.statsStartedAt
              val currentlyUsedXFTPServers = xftpSummary.currentlyUsedXFTPServers
              val previouslyUsedXFTPServers = xftpSummary.previouslyUsedXFTPServers

              XFTPStatsView(totals.stats, statsStartedAt, rh)
              SectionDivider()

              if (currentlyUsedXFTPServers.isNotEmpty()) {
                XftpServersListView(
                  currentlyUsedXFTPServers,
                  statsStartedAt,
                  generalGetString(MR.strings.servers_info_connected_servers_section_header).uppercase(),
                  rh
                )
                SectionDivider()
              }

              if (previouslyUsedXFTPServers.isNotEmpty()) {
                XftpServersListView(
                  previouslyUsedXFTPServers,
                  statsStartedAt,
                  generalGetString(MR.strings.servers_info_previously_connected_servers_section_header).uppercase(),
                  rh
                )
                SectionDivider()
              }

              ServerSessionsView(totals.sessions)
            }

          }
          SectionDivider()

          SectionItemViewSpaceBetween {
            Row {
              Text(
                stringResource(MR.strings.servers_info_reconnect_all_servers_button),
                modifier = Modifier.clickable() {
                  AlertManager.shared.showAlertDialog(
                    title = generalGetString(MR.strings.servers_info_reconnect_servers_title),
                    text = generalGetString(MR.strings.servers_info_reconnect_servers_message),
                    dismissText = generalGetString(MR.strings.servers_info_modal_dismiss),
                    destructive = true,
                    onConfirm = {
                      withBGApi {
                        val success = controller.reconnectAllServers(rh?.remoteHostId)

                        if (!success) {
                          AlertManager.shared.showAlertMsg(
                            title = generalGetString(MR.strings.servers_info_modal_error_title),
                            text = generalGetString(MR.strings.servers_info_reconnect_servers_error)
                          )
                        }
                      }
                    }
                  )
                },
                color = MaterialTheme.colors.primary
              )
            }
          }

          SectionItemViewSpaceBetween {
            Row {
              Text(
                stringResource(MR.strings.servers_info_reset_stats),
                modifier = Modifier.clickable() {
                  AlertManager.shared.showAlertDialog(
                    title = generalGetString(MR.strings.servers_info_reset_stats_alert_title),
                    text = generalGetString(MR.strings.servers_info_reset_stats_alert_message),
                    dismissText = generalGetString(MR.strings.servers_info_modal_dismiss),
                    destructive = true,
                    onConfirm = {
                      withBGApi {
                        val success = controller.resetAgentServersStats(rh?.remoteHostId)

                        if (!success) {
                          AlertManager.shared.showAlertMsg(
                            title = generalGetString(MR.strings.servers_info_modal_error_title),
                            text = generalGetString(MR.strings.servers_info_reset_stats_alert_error_title)
                          )
                        }
                      }
                    }
                  )
                },
                color = MaterialTheme.colors.primary
              )
            }
          }

          SectionBottomSpacer()
        }
      }
    }
  }
}