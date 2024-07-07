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
import androidx.compose.foundation.layout.size
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
import androidx.compose.ui.platform.LocalDensity
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

data class SubscriptionStatus(
  val color: Color,
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

  val activeColor: Color = if (onionHosts == OnionHosts.REQUIRED) Color(0xFF4B0082) else Color(0xFF00796B)
  val noConnColorAndPercent = SubscriptionStatus(Color(0xFFB0BEC5), 1f, 1f, 0f)
  val activeSubsRounded = roundedToQuarter(subs.shareOfActive)

  return if (online && subs.total > 0) {
    if (subs.ssActive == 0) {
      if (sess.ssConnected == 0) noConnColorAndPercent else SubscriptionStatus(activeColor, activeSubsRounded, subs.shareOfActive, subs.shareOfActive)
    } else { // ssActive > 0
      if (sess.ssConnected == 0) SubscriptionStatus(Color(0xFFFFA500), activeSubsRounded, subs.shareOfActive, subs.shareOfActive) // This would mean implementation error
      else SubscriptionStatus(activeColor, activeSubsRounded, subs.shareOfActive, subs.shareOfActive)
    }
  } else noConnColorAndPercent
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
    SubscriptionStatusIcon(color = MaterialTheme.colors.primary, variableValue = statusColorAndPercentage.variableValue)
    if (pref.state.value && !leadingPercentage) Text(percentageText, color = MaterialTheme.colors.secondary)
  }
}

@Composable
fun SubscriptionStatusIndicator(click: (() -> Unit)? = null, ) {
  var subs by remember { mutableStateOf(SMPServerSubs.newSMPServerSubs) }
  var sess by remember { mutableStateOf(ServerSessions.newServerSessions) }
  var timerCounter by remember { mutableStateOf(0) }
  var timer: Job? by remember { mutableStateOf(null) }

  val initialInterval: Duration = 1.seconds
  val regularInterval: Duration = 3.seconds
  val initialPhaseDuration: Duration = 10.seconds

  val scope = rememberCoroutineScope()

  fun setServersSummary() {
    withBGApi {
      val summary: PresentedServersSummary? = chatModel.controller.getAgentServersSummary(chatModel.remoteHostId())

      if (summary != null) {
        subs = summary.allUsersSMP.smpTotals.subs
        sess = summary.allUsersSMP.smpTotals.sessions
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

  Row(if (click != null) Modifier.clickable(onClick = click) else Modifier) {
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
  SectionView(generalGetString(MR.strings.servers_info_transport_sessions_section_header)) {
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
private fun SmpServerView(srvSumm: SMPServerSummary, statsStartedAt: Instant) {
  SectionItemViewSpaceBetween(
    click = {
      ModalManager.start.showCustomModal { _ -> Text("Server") }
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
private fun SmpServersListView(servers: List<SMPServerSummary>, statsStartedAt: Instant, header: String? = null, footer: String? = null) {
  val sortedServers = servers.sortedWith(compareBy<SMPServerSummary> { !it.hasSubs }
    .thenBy { serverAddress(it.smpServer) })

  SectionView(header) {
    sortedServers.map { svr -> SmpServerView(svr, statsStartedAt)}
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
  SectionView(generalGetString(MR.strings.servers_info_statistics_section_header)) {
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
      Text(generalGetString(MR.strings.servers_info_subscriptions_section_header), color = MaterialTheme.colors.secondary, style = MaterialTheme.typography.body2, fontSize = 12.sp)
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
fun XFTPStatsView(stats: AgentXFTPServerStatsData, statsStartedAt: Instant, rh: RemoteHostInfo?) {
  SectionView(generalGetString(MR.strings.servers_info_statistics_section_header)) {
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
  SectionView(generalGetString(MR.strings.servers_info_detailed_statistics_sent_messages_header)) {
    InfoRow(generalGetString(MR.strings.servers_info_detailed_statistics_sent_messages_total), numOrDash(stats._sentDirect + stats._sentViaProxy))
    InfoRowTwoValues("Sent directly", "attempts", stats._sentDirect, stats._sentDirectAttempts)
    InfoRowTwoValues("Sent via proxy", "attempts", stats._sentViaProxy, stats._sentViaProxyAttempts)
    InfoRowTwoValues("Proxied", "attempts", stats._sentProxied, stats._sentProxiedAttempts)
    SectionItemViewSpaceBetween {
      Row {
        Text("Send errors", color = MaterialTheme.colors.onBackground)
      }
    }
    IndentedInfoRow("AUTH", numOrDash(stats._sentAuthErrs))
    IndentedInfoRow("QUOTA", numOrDash(stats._sentQuotaErrs))
    IndentedInfoRow("expired", numOrDash(stats._sentExpiredErrs))
    IndentedInfoRow("other", numOrDash(stats._sentOtherErrs))
  }
  Divider(
    Modifier.padding(
      start = DEFAULT_PADDING_HALF,
      top = 32.dp,
      end = DEFAULT_PADDING_HALF,
      bottom = 30.dp
    )
  )
  SectionView("RECEIVED MESSAGES") {
    InfoRow("Received total", numOrDash(stats._recvMsgs))
    SectionItemViewSpaceBetween {
      Row {
        Text("Receive errors", color = MaterialTheme.colors.onBackground)
      }
    }
    IndentedInfoRow("duplicates", numOrDash(stats._recvDuplicates))
    IndentedInfoRow("decryption errors", numOrDash(stats._recvCryptoErrs))
    IndentedInfoRow("other errors", numOrDash(stats._recvErrs))
    InfoRowTwoValues("Acknowledged", "attempts", stats._ackMsgs, stats._ackAttempts)
    SectionItemViewSpaceBetween {
      Row {
        Text("Acknowledgement errors", color = MaterialTheme.colors.onBackground)
      }
    }
    IndentedInfoRow("NO_MSG errors", numOrDash(stats._ackNoMsgErrs))
    IndentedInfoRow("other errors", numOrDash(stats._ackOtherErrs))
  }
  Divider(
    Modifier.padding(
      start = DEFAULT_PADDING_HALF,
      top = 32.dp,
      end = DEFAULT_PADDING_HALF,
      bottom = 30.dp
    )
  )
  SectionView("CONNECTIONS") {
    InfoRow("Created", numOrDash(stats._connCreated))
    InfoRow("Secured", numOrDash(stats._connCreated))
    InfoRow("Completed", numOrDash(stats._connCompleted))
    InfoRowTwoValues("Deleted", "attempts", stats._connDeleted, stats._connDelAttempts)
    InfoRow("Deletion errors", numOrDash(stats._connDelErrs))
    InfoRowTwoValues("Subscribed", "attempts", stats._connSubscribed, stats._connSubAttempts)
    InfoRow("Subscription results ignored", numOrDash(stats._connSubIgnored))
    InfoRow("Subscription errors", numOrDash(stats._connSubErrs))
  }
  SectionTextFooter(
    String.format(stringResource(MR.strings.servers_info_starting_from), localTimestamp(statsStartedAt))
  )

  SectionBottomSpacer()
}

@Composable
fun DetailedXFTPStatsLayout(stats: AgentXFTPServerStatsData, statsStartedAt: Instant) {
  SectionView("UPLOADED FILES") {
    InfoRow("Size", prettySize(stats._uploadsSize))
    InfoRowTwoValues("Chunks uploaded", "attempts", stats._uploads, stats._uploadAttempts)
    InfoRow("Upload errors", numOrDash(stats._uploadErrs))
    InfoRowTwoValues("Chunks deleted", "attempts", stats._deletions, stats._deleteAttempts)
    InfoRow("Deletion errors", numOrDash(stats._deleteErrs))
  }
  Divider(
    Modifier.padding(
      start = DEFAULT_PADDING_HALF,
      top = 32.dp,
      end = DEFAULT_PADDING_HALF,
      bottom = 30.dp
    )
  )
  SectionView("DOWNLOADED FILES") {
    InfoRow("Size", prettySize(stats._downloadsSize))
    InfoRowTwoValues("Chunks downloaded", "attempts", stats._downloads, stats._downloadAttempts)
    SectionItemViewSpaceBetween {
      Row {
        Text("Download errors", color = MaterialTheme.colors.onBackground)
      }
    }
    IndentedInfoRow("AUTH", numOrDash(stats._downloadAuthErrs))
    IndentedInfoRow("other", numOrDash(stats._downloadErrs))
  }
  SectionTextFooter(
    String.format(stringResource(MR.strings.servers_info_starting_from), localTimestamp(statsStartedAt))
  )

  SectionBottomSpacer()
}

@Composable
fun XFTPServerSummaryLayout(summary: XFTPServerSummary, statsStartedAt: Instant, rh: RemoteHostInfo?) {
  SectionView("SERVER ADDRESS") {
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
        "Open server settings",
        modifier = Modifier.padding(DEFAULT_PADDING).clickable() {
          ModalManager.start.showCustomModal { close -> ProtocolServersView(chatModel, rhId = rh?.remoteHostId, ServerProtocol.XFTP, close) }
        },
        color = MaterialTheme.colors.primary
      )
    }

    if (summary.stats != null) {
      Divider(
        Modifier.padding(
          start = DEFAULT_PADDING_HALF,
          top = 32.dp,
          end = DEFAULT_PADDING_HALF,
          bottom = 30.dp
        )
      )
      XFTPStatsView(stats = summary.stats, rh = rh, statsStartedAt = statsStartedAt)
    }

    if (summary.sessions != null) {
      Divider(
        Modifier.padding(
          start = DEFAULT_PADDING_HALF,
          top = 32.dp,
          end = DEFAULT_PADDING_HALF,
          bottom = 30.dp
        )
      )
      ServerSessionsView(summary.sessions)
    }
  }

  SectionBottomSpacer()
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
    var serversSummary by remember { mutableStateOf<PresentedServersSummary?>(null) }
    //val selectedUserCategory = remember { stateGetOrPut("selection") { PresentedUserCategory.ALL_USERS } }
    val selectedServerType = remember { stateGetOrPut("serverTypeSelection") { PresentedServerType.SMP } }

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
        //selectedUserCategory.value = PresentedUserCategory.CURRENT_USER
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
        return Text("No info, try to reload")
      }

//      val userTabTitles = PresentedUserCategory.entries.map {
//        when (it) {
//          PresentedUserCategory.CURRENT_USER ->
//            stringResource(MR.strings.current_user)
//
//          PresentedUserCategory.ALL_USERS ->
//            stringResource(MR.strings.all_users)
//        }
//      }

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
                if (PresentedServerType.SMP.ordinal == index) painterResource(MR.images.ic_mail) else painterResource(MR.images.ic_download),
                it
              )
            },
            selectedContentColor = MaterialTheme.colors.primary,
            unselectedContentColor = MaterialTheme.colors.secondary,
          )
        }
      }

      HorizontalPager(state = serverTypePagerState, Modifier.fillMaxSize(), verticalAlignment = Alignment.Top) { index ->
        ColumnWithScrollBar(
          Modifier
            .fillMaxSize(),
          verticalArrangement = Arrangement.Top) {
          Spacer(Modifier.height(DEFAULT_PADDING))
          when (index) {
            PresentedServerType.SMP.ordinal -> {
              val castedSummary = serversSummary!!
              val smpSummary = castedSummary.currentUserSMP;
              val totals = smpSummary.smpTotals
              val currentlyUsedSMPServers = smpSummary.currentlyUsedSMPServers
              val previouslyUsedSMPServers = smpSummary.previouslyUsedSMPServers
              val proxySMPServers = smpSummary.onlyProxiedSMPServers
              val statsStartedAt = castedSummary.statsStartedAt

              SMPStatsView(totals.stats, statsStartedAt, rh)
              Divider(
                Modifier.padding(
                  start = DEFAULT_PADDING_HALF,
                  top = 32.dp,
                  end = DEFAULT_PADDING_HALF,
                  bottom = 30.dp
                )
              )
              SMPSubscriptionsSection(totals)
              Divider(
                Modifier.padding(
                  start = DEFAULT_PADDING_HALF,
                  top = 32.dp,
                  end = DEFAULT_PADDING_HALF,
                  bottom = 30.dp
                )
              )

              if (currentlyUsedSMPServers.isNotEmpty()) {
                SmpServersListView(currentlyUsedSMPServers, statsStartedAt, generalGetString(MR.strings.servers_info_connected_servers_section_header))
                Divider(
                  Modifier.padding(
                    start = DEFAULT_PADDING_HALF,
                    top = 32.dp,
                    end = DEFAULT_PADDING_HALF,
                    bottom = 30.dp
                  )
                )
              }

              if (previouslyUsedSMPServers.isNotEmpty()) {
                SmpServersListView(previouslyUsedSMPServers, statsStartedAt, generalGetString(MR.strings.servers_info_previously_connected_servers_section_header))
                Divider(
                  Modifier.padding(
                    start = DEFAULT_PADDING_HALF,
                    top = 32.dp,
                    end = DEFAULT_PADDING_HALF,
                    bottom = 30.dp
                  )
                )
              }

              if (proxySMPServers.isNotEmpty()) {
                SmpServersListView(proxySMPServers, statsStartedAt, generalGetString(MR.strings.servers_info_proxied_servers_section_header), generalGetString(MR.strings.servers_info_proxied_servers_section_footer))
                Divider(
                  Modifier.padding(
                    start = DEFAULT_PADDING_HALF,
                    top = 32.dp,
                    end = DEFAULT_PADDING_HALF,
                    bottom = 30.dp
                  )
                )
              }

              ServerSessionsView(totals.sessions)
            }

            PresentedServerType.XFTP.ordinal -> {
              val castedSummary = serversSummary!!
              val xftpSummary = castedSummary.currentUserXFTP
              val totals = xftpSummary.xftpTotals
              val statsStartedAt = castedSummary.statsStartedAt
              val currentlyUsedXFTPServers = xftpSummary.currentlyUsedXFTPServers
              val previouslyUsedXFTPServers = xftpSummary.previouslyUsedXFTPServers

              XFTPStatsView(totals.stats, statsStartedAt, rh)
              Divider(
                Modifier.padding(
                  start = DEFAULT_PADDING_HALF,
                  top = 32.dp,
                  end = DEFAULT_PADDING_HALF,
                  bottom = 30.dp
                )
              )

              if (currentlyUsedXFTPServers.isNotEmpty()) {
                XftpServersListView(currentlyUsedXFTPServers, statsStartedAt, generalGetString(MR.strings.servers_info_connected_servers_section_header), rh)
                Divider(
                  Modifier.padding(
                    start = DEFAULT_PADDING_HALF,
                    top = 32.dp,
                    end = DEFAULT_PADDING_HALF,
                    bottom = 30.dp
                  )
                )
              }

              if (previouslyUsedXFTPServers.isNotEmpty()) {
                XftpServersListView(previouslyUsedXFTPServers, statsStartedAt, generalGetString(MR.strings.servers_info_previously_connected_servers_section_header), rh)
                Divider(
                  Modifier.padding(
                    start = DEFAULT_PADDING_HALF,
                    top = 32.dp,
                    end = DEFAULT_PADDING_HALF,
                    bottom = 30.dp
                  )
                )
              }

              ServerSessionsView(totals.sessions)
            }

          }
          Divider(
            Modifier.padding(
              start = DEFAULT_PADDING_HALF,
              top = 32.dp,
              end = DEFAULT_PADDING_HALF,
              bottom = 30.dp
            )
          )

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