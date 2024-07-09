package chat.simplex.common.views.chatlist

import InfoRow
import InfoRowTwoValues
import SectionBottomSpacer
import SectionDividerSpaced
import SectionItemView
import SectionTextFooter
import SectionView
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
import androidx.compose.foundation.layout.size
import androidx.compose.foundation.pager.HorizontalPager
import androidx.compose.foundation.pager.rememberPagerState
import androidx.compose.foundation.text.selection.SelectionContainer
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.text.TextStyle
import androidx.compose.ui.text.font.FontFamily
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import chat.simplex.common.model.AgentSMPServerStatsData
import chat.simplex.common.model.AgentXFTPServerStatsData
import chat.simplex.common.model.ChatController.chatModel
import chat.simplex.common.model.ChatModel.controller
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
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.helpers.*
import chat.simplex.common.views.usersettings.ProtocolServersView
import chat.simplex.res.MR
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import kotlinx.coroutines.cancel
import kotlinx.coroutines.launch
import kotlinx.datetime.Instant
import numOrDash
import java.text.DecimalFormat
import kotlin.math.floor
import kotlin.math.roundToInt

enum class SubscriptionColorType {
  ACTIVE, ACTIVE_SOCKS_PROXY, DISCONNECTED, ACTIVE_DISCONNECTED
}

data class SubscriptionStatus(
  val color: SubscriptionColorType,
  val variableValue: Float,
  val opacity: Float,
  val statusPercent: Float
)

fun subscriptionStatusColorAndPercentage(
  online: Boolean,
  socksProxy: String?,
  subs: SMPServerSubs,
  sess: ServerSessions
): SubscriptionStatus {

  fun roundedToQuarter(n: Float): Float = when {
    n >= 1 -> 1f
    n <= 0 -> 0f
    else -> (n * 4).roundToInt() / 4f
  }

  val activeColor: SubscriptionColorType = if (socksProxy != null) SubscriptionColorType.ACTIVE_SOCKS_PROXY else SubscriptionColorType.ACTIVE
  val noConnColorAndPercent = SubscriptionStatus(SubscriptionColorType.DISCONNECTED, 1f, 1f, 0f)
  val activeSubsRounded = roundedToQuarter(subs.shareOfActive)

  return if (online && subs.total > 0) {
    if (subs.ssActive == 0) {
      if (sess.ssConnected == 0)
        noConnColorAndPercent
      else
        SubscriptionStatus(activeColor, activeSubsRounded, subs.shareOfActive, subs.shareOfActive)
    } else { // ssActive > 0
      if (sess.ssConnected == 0)
        // This would mean implementation error
        SubscriptionStatus(SubscriptionColorType.ACTIVE_DISCONNECTED, activeSubsRounded, subs.shareOfActive, subs.shareOfActive)
      else
        SubscriptionStatus(activeColor, activeSubsRounded, subs.shareOfActive, subs.shareOfActive)
    }
  } else noConnColorAndPercent
}

@Composable
private fun SubscriptionStatusIndicatorPercentage(percentageText: String) {
  Text(
    percentageText,
    color = MaterialTheme.colors.secondary,
    fontSize = 12.sp,
    style = MaterialTheme.typography.caption
  )
}

@Composable
fun SubscriptionStatusIndicatorView(subs: SMPServerSubs, sess: ServerSessions, leadingPercentage: Boolean = false) {
  val netCfg = rememberUpdatedState(chatModel.controller.getNetCfg())
  val statusColorAndPercentage = subscriptionStatusColorAndPercentage(chatModel.networkInfo.value.online, netCfg.value.socksProxy, subs, sess)
  val pref = remember { chatModel.controller.appPrefs.networkShowSubscriptionPercentage }
  val percentageText = "${(floor(statusColorAndPercentage.statusPercent * 100)).toInt()}%"

  Row(
    verticalAlignment = Alignment.CenterVertically,
    horizontalArrangement = Arrangement.spacedBy(DEFAULT_SPACE_AFTER_ICON)
  ) {
    if (pref.state.value && leadingPercentage) SubscriptionStatusIndicatorPercentage(percentageText)
    SubscriptionStatusIcon(
      color = when(statusColorAndPercentage.color) {
        SubscriptionColorType.ACTIVE -> MaterialTheme.colors.primary
        SubscriptionColorType.ACTIVE_SOCKS_PROXY -> Indigo
        SubscriptionColorType.ACTIVE_DISCONNECTED -> WarningOrange
        SubscriptionColorType.DISCONNECTED -> MaterialTheme.colors.secondary
      },
      modifier = Modifier.size(16.dp),
      variableValue = statusColorAndPercentage.variableValue)
    if (pref.state.value && !leadingPercentage) SubscriptionStatusIndicatorPercentage(percentageText)
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
      generalGetString(MR.strings.servers_info_sessions_connected),
      numOrDash(sess.ssConnected)
    )
    InfoRow(
      generalGetString(MR.strings.servers_info_sessions_errors),
      numOrDash(sess.ssErrors)
    )
    InfoRow(
      generalGetString(MR.strings.servers_info_sessions_connecting),
      numOrDash(sess.ssConnecting)
    )
  }
}

private fun serverAddress(server: String): String {
  val address =  parseServerAddress(server)

  return address?.hostnames?.first() ?: server
}

@Composable
private fun SMPServerView(srvSumm: SMPServerSummary, statsStartedAt: Instant, rh: RemoteHostInfo?) {
  SectionItemView(
    click = {
      ModalManager.start.showCustomModal { close ->
        SMPServerSummaryView(
          rh = rh,
          close = close,
          summary = srvSumm,
          statsStartedAt = statsStartedAt
        )
      }
    }
  ) {
    Text(
      serverAddress(srvSumm.smpServer),
      modifier = Modifier.weight(10f, fill = true)
    )
    if (srvSumm.subs != null) {
      Spacer(Modifier.fillMaxWidth().weight(1f))
      SubscriptionStatusIndicatorView(subs = srvSumm.subs, sess = srvSumm.sessionsOrNew, leadingPercentage = true)
    } else if (srvSumm.sessions != null) {
      Spacer(Modifier.fillMaxWidth().weight(1f))
      Icon(painterResource(MR.images.ic_arrow_upward), contentDescription = null, tint = SessIconColor(srvSumm.sessions))
    }
  }
}

@Composable
private fun SessIconColor(sess: ServerSessions): Color {
  val online = chatModel.networkInfo.value.online
  return if (online && sess.ssConnected > 0) SessionActiveColor() else MaterialTheme.colors.secondary
}

@Composable
private fun SessionActiveColor(): Color {
  val netCfg = rememberUpdatedState(chatModel.controller.getNetCfg())
  return if (netCfg.value.socksProxy != null) Indigo else MaterialTheme.colors.primary
}

@Composable
private fun SMPServersListView(servers: List<SMPServerSummary>, statsStartedAt: Instant, header: String? = null, footer: String? = null, rh: RemoteHostInfo?) {
  val sortedServers = servers.sortedWith(compareBy<SMPServerSummary> { !it.hasSubs }
    .thenBy { serverAddress(it.smpServer) })

  SectionView(header) {
    sortedServers.map { svr -> SMPServerView(srvSumm = svr, statsStartedAt = statsStartedAt, rh = rh) }
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
private fun XFTPServerView(srvSumm: XFTPServerSummary, statsStartedAt: Instant, rh: RemoteHostInfo?) {
  SectionItemView(
    click = {
      ModalManager.start.showCustomModal { close ->
        XFTPServerSummaryView(
          rh = rh,
          close = close,
          summary = srvSumm,
          statsStartedAt = statsStartedAt
        )
      }
    }
  ) {
    Text(
      serverAddress(srvSumm.xftpServer),
      modifier = Modifier.weight(10f, fill = true)
    )
    if (srvSumm.rcvInProgress || srvSumm.sndInProgress || srvSumm.delInProgress) {
      Spacer(Modifier.fillMaxWidth().weight(1f))
      XFTPServerInProgressIcon(srvSumm)
    }
  }
}

@Composable
private fun XFTPServerInProgressIcon(srvSumm: XFTPServerSummary) {
  return when {
    srvSumm.rcvInProgress && !srvSumm.sndInProgress && !srvSumm.delInProgress -> Icon(painterResource(MR.images.ic_arrow_downward),"download", tint = SessionActiveColor())
    !srvSumm.rcvInProgress && srvSumm.sndInProgress && !srvSumm.delInProgress -> Icon(painterResource(MR.images.ic_arrow_upward), "upload", tint = SessionActiveColor())
    !srvSumm.rcvInProgress && !srvSumm.sndInProgress && srvSumm.delInProgress -> Icon(painterResource(MR.images.ic_delete), "deletion", tint = SessionActiveColor())
    else -> Icon(painterResource(MR.images.ic_expand_all), "upload and download", tint = SessionActiveColor())
  }
}

@Composable
private fun XFTPServersListView(servers: List<XFTPServerSummary>, statsStartedAt: Instant, header: String? = null, rh: RemoteHostInfo?) {
  val sortedServers = servers.sortedBy { serverAddress(it.xftpServer) }

  SectionView(header) {
    sortedServers.map { svr -> XFTPServerView(svr, statsStartedAt, rh) }
  }
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
    SectionItemView(
      click = {
        ModalManager.start.showCustomModal { close -> DetailedSMPStatsView(
          rh = remoteHostInfo,
          close = close,
          stats = stats,
          statsStartedAt = statsStartedAt)
        }
      }
    ) {
      Text(text = generalGetString(MR.strings.servers_info_details), color = MaterialTheme.colors.onBackground)
    }
  }
  SectionTextFooter(
    String.format(stringResource(MR.strings.servers_info_private_data_disclaimer), localTimestamp(statsStartedAt))
  )
}

@Composable
private fun SMPSubscriptionsSection(totals: SMPTotals) {
  Column {
    Row(
      Modifier.padding(start = DEFAULT_PADDING, bottom = 5.dp),
      verticalAlignment = Alignment.CenterVertically,
      horizontalArrangement = Arrangement.spacedBy(DEFAULT_SPACE_AFTER_ICON * 2)
    ) {
      Text(
        generalGetString(MR.strings.servers_info_subscriptions_section_header).uppercase(),
        color = MaterialTheme.colors.secondary,
        style = MaterialTheme.typography.body2,
        fontSize = 12.sp
      )
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
    Row(
      Modifier.padding(start = DEFAULT_PADDING, bottom = 5.dp),
      verticalAlignment = Alignment.CenterVertically,
      horizontalArrangement = Arrangement.spacedBy(DEFAULT_SPACE_AFTER_ICON * 2)
    ) {
      Text(
        generalGetString(MR.strings.servers_info_subscriptions_section_header).uppercase(),
        color = MaterialTheme.colors.secondary,
        style = MaterialTheme.typography.body2,
        fontSize = 12.sp
      )
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
      ReconnectServerButton(rh, summary.smpServer)
    }
  }
}

@Composable
private fun ReconnectServerButton(rh: RemoteHostInfo?, server: String) {
  SectionItemView(click = { reconnectServerAlert(rh, server) }) {
    Text(
      stringResource(MR.strings.reconnect),
      color = MaterialTheme.colors.primary
    )
  }
}

private fun reconnectServerAlert(rh: RemoteHostInfo?, server: String) {
  AlertManager.shared.showAlertDialog(
    title = generalGetString(MR.strings.servers_info_reconnect_server_title),
    text = generalGetString(MR.strings.servers_info_reconnect_server_message),
    onConfirm = {
      withBGApi {
        val success = controller.reconnectServer(rh?.remoteHostId, server)

        if (!success) {
          AlertManager.shared.showAlertMsg(
            title = generalGetString(MR.strings.servers_info_modal_error_title),
            text = generalGetString(MR.strings.servers_info_reconnect_server_error)
          )
        }
      }
    }
  )
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
    SectionItemView (
      click = {
        ModalManager.start.showCustomModal { close -> DetailedXFTPStatsView(
          rh = rh,
          close = close,
          stats = stats,
          statsStartedAt = statsStartedAt)
        }
      }
    ) {
      Text(text = generalGetString(MR.strings.servers_info_details), color = MaterialTheme.colors.onBackground)
    }
  }
  SectionTextFooter(
    String.format(stringResource(MR.strings.servers_info_private_data_disclaimer), localTimestamp(statsStartedAt))
  )
}

@Composable
private fun IndentedInfoRow(title: String, desc: String) {
  InfoRow(title, desc, padding = PaddingValues(start = 24.dp + DEFAULT_PADDING, end = DEFAULT_PADDING))
}

@Composable
fun DetailedSMPStatsLayout(stats: AgentSMPServerStatsData, statsStartedAt: Instant) {
  SectionView(generalGetString(MR.strings.servers_info_detailed_statistics_sent_messages_header).uppercase()) {
    InfoRow(generalGetString(MR.strings.servers_info_detailed_statistics_sent_messages_total), numOrDash(stats._sentDirect + stats._sentViaProxy))
    InfoRowTwoValues(generalGetString(MR.strings.sent_directly), generalGetString(MR.strings.attempts_label), stats._sentDirect, stats._sentDirectAttempts)
    InfoRowTwoValues(generalGetString(MR.strings.sent_via_proxy), generalGetString(MR.strings.attempts_label), stats._sentViaProxy, stats._sentViaProxyAttempts)
    InfoRowTwoValues(generalGetString(MR.strings.proxied), generalGetString(MR.strings.attempts_label), stats._sentProxied, stats._sentProxiedAttempts)
    SectionItemView {
      Text(generalGetString(MR.strings.send_errors), color = MaterialTheme.colors.onBackground)
    }
    IndentedInfoRow("AUTH", numOrDash(stats._sentAuthErrs))
    IndentedInfoRow("QUOTA", numOrDash(stats._sentQuotaErrs))
    IndentedInfoRow(generalGetString(MR.strings.expired_label), numOrDash(stats._sentExpiredErrs))
    IndentedInfoRow(generalGetString(MR.strings.other_label), numOrDash(stats._sentOtherErrs))
  }

  SectionDividerSpaced()

  SectionView(generalGetString(MR.strings.servers_info_detailed_statistics_received_messages_header).uppercase()) {
    InfoRow(generalGetString(MR.strings.servers_info_detailed_statistics_received_total), numOrDash(stats._recvMsgs))
    SectionItemView {
      Text(generalGetString(MR.strings.servers_info_detailed_statistics_receive_errors), color = MaterialTheme.colors.onBackground)
    }
    IndentedInfoRow(generalGetString(MR.strings.duplicates_label), numOrDash(stats._recvDuplicates))
    IndentedInfoRow(generalGetString(MR.strings.decryption_errors), numOrDash(stats._recvCryptoErrs))
    IndentedInfoRow(generalGetString(MR.strings.other_errors), numOrDash(stats._recvErrs))
    InfoRowTwoValues(generalGetString(MR.strings.acknowledged), generalGetString(MR.strings.attempts_label), stats._ackMsgs, stats._ackAttempts)
    SectionItemView {
      Text(generalGetString(MR.strings.acknowledgement_errors), color = MaterialTheme.colors.onBackground)
    }
    IndentedInfoRow("NO_MSG errors", numOrDash(stats._ackNoMsgErrs))
    IndentedInfoRow(generalGetString(MR.strings.other_errors), numOrDash(stats._ackOtherErrs))
  }

  SectionDividerSpaced()

  SectionView(generalGetString(MR.strings.connections).uppercase()) {
    InfoRow(generalGetString(MR.strings.created), numOrDash(stats._connCreated))
    InfoRow(generalGetString(MR.strings.secured), numOrDash(stats._connSecured))
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
  SectionDividerSpaced()
  SectionView(generalGetString(MR.strings.downloaded_files).uppercase()) {
    InfoRow(generalGetString(MR.strings.size), prettySize(stats._downloadsSize))
    InfoRowTwoValues(generalGetString(MR.strings.chunks_downloaded), generalGetString(MR.strings.attempts_label), stats._downloads, stats._downloadAttempts)
    SectionItemView {
      Text(generalGetString(MR.strings.download_errors), color = MaterialTheme.colors.onBackground)
    }
    IndentedInfoRow("AUTH", numOrDash(stats._downloadAuthErrs))
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
      SectionItemView(click = {
        ModalManager.start.showCustomModal { close -> ProtocolServersView(chatModel, rhId = rh?.remoteHostId, ServerProtocol.XFTP, close) }
      }) {
        Text(generalGetString(MR.strings.open_server_settings_button))
      }
    }

    if (summary.stats != null) {
      SectionDividerSpaced()
      XFTPStatsView(stats = summary.stats, rh = rh, statsStartedAt = statsStartedAt)
    }

    if (summary.sessions != null) {
      SectionDividerSpaced()
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
      SectionItemView(click = {
        ModalManager.start.showCustomModal { close -> ProtocolServersView(chatModel, rhId = rh?.remoteHostId, ServerProtocol.SMP, close) }
      }) {
        Text(generalGetString(MR.strings.open_server_settings_button))
      }
    }

    if (summary.stats != null) {
      SectionDividerSpaced()
      SMPStatsView(stats = summary.stats, remoteHostInfo = rh, statsStartedAt = statsStartedAt)
    }

    if (summary.subs != null) {
      SectionDividerSpaced()
      SMPSubscriptionsSection(subs = summary.subs, summary = summary, rh = rh)
    }

    if (summary.sessions != null) {
      SectionDividerSpaced()
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
    close = close
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
    close = close
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
    close = close
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
    close = close
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
fun ModalData.ServersSummaryView(rh: RemoteHostInfo?, serversSummary: MutableState<PresentedServersSummary?>) {
  Column(
    Modifier.fillMaxSize(),
  ) {
    var showUserSelection by remember { mutableStateOf(false) }
    val selectedUserCategory =
      remember { stateGetOrPut("selectedUserCategory") { PresentedUserCategory.ALL_USERS } }
    val selectedServerType =
      remember { stateGetOrPut("serverTypeSelection") { PresentedServerType.SMP } }
    val scope = rememberCoroutineScope()

    LaunchedEffect(Unit) {
      if (chatModel.users.count { u -> u.user.activeUser || !u.user.hidden } == 1
      ) {
        selectedUserCategory.value = PresentedUserCategory.CURRENT_USER
      } else {
        showUserSelection = true
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
      if (serversSummary.value == null) {
        Box(
          modifier = Modifier
            .fillMaxSize()
        ) {
          Text(generalGetString(MR.strings.servers_info_missing), Modifier.align(Alignment.Center), color = MaterialTheme.colors.secondary)
        }
      } else {
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
              stringResource(MR.strings.servers_info_files_tab)
          }
        }
        val serverTypePagerState = rememberPagerState(
          initialPage = selectedServerType.value.ordinal,
          initialPageOffsetFraction = 0f
        ) { PresentedServerType.entries.size }

        KeyChangeEffect(serverTypePagerState.currentPage) {
          selectedServerType.value = PresentedServerType.values()[serverTypePagerState.currentPage]
        }
        TabRow(
          selectedTabIndex = serverTypePagerState.currentPage,
          backgroundColor = Color.Transparent,
          contentColor = MaterialTheme.colors.primary,
        ) {
          serverTypeTabTitles.forEachIndexed { index, it ->
            Tab(
              selected = serverTypePagerState.currentPage == index,
              onClick = {
                scope.launch {
                  serverTypePagerState.animateScrollToPage(index)
                }
              },
              text = { Text(it, fontSize = 13.sp) },
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
              SectionDividerSpaced()
            }
            when (index) {
              PresentedServerType.SMP.ordinal -> {
                serversSummary.value?.let {
                  val smpSummary =
                    if (selectedUserCategory.value == PresentedUserCategory.CURRENT_USER) it.currentUserSMP else it.allUsersSMP;
                  val totals = smpSummary.smpTotals
                  val currentlyUsedSMPServers = smpSummary.currentlyUsedSMPServers
                  val previouslyUsedSMPServers = smpSummary.previouslyUsedSMPServers
                  val proxySMPServers = smpSummary.onlyProxiedSMPServers
                  val statsStartedAt = it.statsStartedAt

                  SMPStatsView(totals.stats, statsStartedAt, rh)
                  SectionDividerSpaced()
                  SMPSubscriptionsSection(totals)
                  SectionDividerSpaced()

                  if (currentlyUsedSMPServers.isNotEmpty()) {
                    SMPServersListView(
                      servers = currentlyUsedSMPServers,
                      statsStartedAt = statsStartedAt,
                      header = generalGetString(MR.strings.servers_info_connected_servers_section_header).uppercase(),
                      rh = rh
                    )
                    SectionDividerSpaced()
                  }

                  if (previouslyUsedSMPServers.isNotEmpty()) {
                    SMPServersListView(
                      servers = previouslyUsedSMPServers,
                      statsStartedAt = statsStartedAt,
                      header = generalGetString(MR.strings.servers_info_previously_connected_servers_section_header).uppercase(),
                      rh = rh
                    )
                    SectionDividerSpaced()
                  }

                  if (proxySMPServers.isNotEmpty()) {
                    SMPServersListView(
                      servers = proxySMPServers,
                      statsStartedAt = statsStartedAt,
                      header = generalGetString(MR.strings.servers_info_proxied_servers_section_header).uppercase(),
                      footer = generalGetString(MR.strings.servers_info_proxied_servers_section_footer),
                      rh = rh
                    )
                    SectionDividerSpaced()
                  }

                  ServerSessionsView(totals.sessions)
                }
              }

              PresentedServerType.XFTP.ordinal -> {
                serversSummary.value?.let {
                  val xftpSummary =
                    if (selectedUserCategory.value == PresentedUserCategory.CURRENT_USER) it.currentUserXFTP else it.allUsersXFTP
                  val totals = xftpSummary.xftpTotals
                  val statsStartedAt = it.statsStartedAt
                  val currentlyUsedXFTPServers = xftpSummary.currentlyUsedXFTPServers
                  val previouslyUsedXFTPServers = xftpSummary.previouslyUsedXFTPServers

                  XFTPStatsView(totals.stats, statsStartedAt, rh)
                  SectionDividerSpaced()

                  if (currentlyUsedXFTPServers.isNotEmpty()) {
                    XFTPServersListView(
                      currentlyUsedXFTPServers,
                      statsStartedAt,
                      generalGetString(MR.strings.servers_info_connected_servers_section_header).uppercase(),
                      rh
                    )
                    SectionDividerSpaced()
                  }

                  if (previouslyUsedXFTPServers.isNotEmpty()) {
                    XFTPServersListView(
                      previouslyUsedXFTPServers,
                      statsStartedAt,
                      generalGetString(MR.strings.servers_info_previously_connected_servers_section_header).uppercase(),
                      rh
                    )
                    SectionDividerSpaced()
                  }

                  ServerSessionsView(totals.sessions)
                }
              }
            }

            SectionDividerSpaced()

            SectionView {
              ReconnectAllServersButton(rh)
              ResetStatisticsButton(rh)
            }

            SectionBottomSpacer()
          }
        }
      }
    }
  }
}

@Composable
private fun ReconnectAllServersButton(rh: RemoteHostInfo?) {
  SectionItemView(click = { reconnectAllServersAlert(rh) }) {
    Text(
      stringResource(MR.strings.servers_info_reconnect_all_servers_button),
      color = MaterialTheme.colors.primary
    )
  }
}

private fun reconnectAllServersAlert(rh: RemoteHostInfo?) {
  AlertManager.shared.showAlertDialog(
    title = generalGetString(MR.strings.servers_info_reconnect_servers_title),
    text = generalGetString(MR.strings.servers_info_reconnect_servers_message),
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
}

@Composable
private fun ResetStatisticsButton(rh: RemoteHostInfo?) {
  SectionItemView(click = { resetStatisticsAlert(rh) }) {
    Text(
      stringResource(MR.strings.servers_info_reset_stats),
      color = MaterialTheme.colors.primary
    )
  }
}

private fun resetStatisticsAlert(rh: RemoteHostInfo?) {
  AlertManager.shared.showAlertDialog(
    title = generalGetString(MR.strings.servers_info_reset_stats_alert_title),
    text = generalGetString(MR.strings.servers_info_reset_stats_alert_message),
    confirmText = generalGetString(MR.strings.servers_info_reset_stats_alert_confirm),
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
}
